# Asynchronous streams.
deque <- function(len=64) {
  data <- vector("list", min(len, 2))
  i.first <- 1
  i.open <- 1

  getFirst <- function(or=stop("StopIteration")) {
    if (i.open == i.first) {
      or
    } else {
      val <- data[[i.first]]
      data[[i.first]] <<- "xxx"
      i.first <<- (i.first %% length(data)) + 1
      val
    }
  }

  getLast <- function(or=stop("StopIteration")) {
    if (i.open == i.first) {
      or
    } else {
      if (i.open == 1) i.open <<- length(data)
      else i.open <<- i.open - 1
      val <- data[[i.open]]
      data[[i.open]] <<- "xxx"
      val
    }
  }

  `%:%` <- function(from, to) {
    seq_len(to - from) + (from - 1)
  }

  expand <- function() {
    data2 <- vector(mode(data), 2 * length(data))
    if (i.open <= i.first) {
      slice1 <- 1 %:% i.open
      slice2 <- i.first %:% (length(data)+1)
      data2[slice1] <- data[slice1]
      data2[slice2 + length(data)] <- data[slice2]
      i.first <<- i.first + length(data)
    } else {
      slice <- i.first %:% i.open
      data2[slice] <- data[slice]
    }
    data <<- data2
  }

  append <- function(val) {
    if ((i.open %% length(data)) + 1 == i.first) expand()
    data[[i.open]] <<- val
    i.open <<- (i.open %% length(data)) + 1
  }

  prepend <- function(val) {
    if ((i.open %% length(data)) + 1 == i.first) expand()
    if (i.first == 1) i.first <<- length(data)
    else i.first <<- i.first-1
    data[[i.first]] <<- val
  }

  getLength <- function() {
    if (i.open < i.first) {
      i.open - i.first + length(data)
    } else {
      i.open - i.first
    }
  }

  structure(list(append=append,
                 prepend=prepend,
                 nextOr=getFirst,
                 getFirst=getFirst,
                 getLast=getLast,
                 length=getLength),
            class=c("deque", "funiteror", "iteror", "iter"))
}


#' An object representing a sequence of future values.
#'
#' A `channel` is an object that represents a sequence of values yet
#' to be determined. It is something like a combination of a [promise]
#' and an [iteror].
#'
#' The channel interface is intended to represent and work with
#' asynchronous, live data sources, for instance event logs,
#' non-blocking connections, paginated query results, reactive values,
#' and other processes that yield a sequence of values over time.
#'
#' `channel` is an S3 method and will attempt to convert the argument
#' `obj` into a channel object according to its class. In particular
#' [connection] objects will be wrapped with a connection.
#'
#' The friendly way to obtain values from a channel is to use
#' `awaitNext` or `for` loops within an [async] or [stream] coroutine.
#'
#' The low-level interface to obtain values from a channel is to call
#' [nextThen]`(ch, onNext=, onError=, onClose=, ...)]`, providing callback
#' functions for at least `onNext(val)`. Those callbacks will be
#' appended to an internal queue, and will be called as soon as data
#' is available, in the order that requests were received.
#'
#' You can also treat a channel as an [iteror] over promises, calling
#' `nextOr(pri)` to return a [promise] representing the next
#' available value. Each promise created this way will be resolved in
#' the order that data come in. Note that this way there is no special
#' signal for end of iteration; a promise will reject with
#' a condition message `"StopIteration"` to signal end of iteration.
#'
#' Be careful with the iterator-over-promises interface though: if you
#' call `as.list.iteror(pr)` you may get stuck in an infinite loop, as
#' `as.list` keeps calling `nextElem` and receives more promises to
#' represent values that exist only hypothetically. This is one
#' reason for the `max_listeners` limit.
#'
#' The friendly way to create a channel with custom behavior is to use
#' a [stream] coroutine. Inside of `stream()` call [await] to wait on
#' promises, [awaitNext] to wait on other streams and [yield] to yield
#' values. To signal end of iteration use `return()` (which will
#' discard its value) and to signal an error use `stop()`.
#'
#' The low-level interface to create a channel with custom behavior
#' is to call `channel(function(emit, reject, cancel) {...})`,
#' providing your own function definition; your function will
#' receive those three callback methods as arguments. Then use
#' whatever means to arrange to call `emit(val)` some time in the
#' future as data comes in. When you are done emitting values, call
#' the `close()` callback. To report an error call
#' `reject(err)`; the next requestor will receive the error. If there
#' is more than one listener, other queued listeners will get a
#' `close` signal.
#'
#' @param obj A user-provided function; it will receive three
#'   callback functions as arguments, in order, `emit(val)`,
#'   `reject(err)` and `close()`
#' @param max_queue The maximum number of outgoing values to store if
#'   there are no listeners. Beyond this, calling `emit` will return
#'   an error.
#' @param max_awaiting The maximum number of pending requests. If
#'   there are this many outstanding requests, for values, calling
#'   `nextThen(ch, ...)` or `nextElem(ch)` will raise an error.
#' @param wakeup You may optionally provide a callback function here.
#'   It will be called when the queue is empty and there is at least
#'   one listener/outstanding promise.
#' @return a channel object, supporting methods "nextThen" and "nextElem"
#'
#' @author Peter Meilstrup
#' @export
channel <- function(obj, ...) {
  UseMethod("channel")
}

#' @exportS3Method
channel.default <- function(obj, ...) {
  if (is.function(obj))
    channel.function(obj, ...)
  else stop("Don't know how to make channel out of that")
}

#' @exportS3Method channel "function"
#' @export
#' @rdname channel
channel.function <- function(obj, max_queue=500L, max_awaiting=500L,
                             wakeup=function(...) NULL) {
  # list of callbacks waiting to be made having yet to be sent
  # each is a list(resolve=, reject=, close= )
  outgoing <- deque()
  # list of values waiting for a callback
  awaiting <- deque()
  state <- "running"
  errorValue <- NULL
  odo <- 0

  emit <- function(val) {
    if (state != "running")
      stop("Can not emit; state is ", state)
    if (outgoing$length() >= max_queue)
      stop("Outgoing queue is full!")
    outgoing$append(val)
    send()
    invisible(val)
  }

  reject <- function(...) {
    state <<- "error"
    if (inherits(..1, "condition")) {
      errorValue <<- ..1
      if (nargs() > 1L)
        warning("additional arguments ignored in reject()")
    } else {
      errorValue <<- simpleError(...)
    }
    send()
  }

  close <- function() {
    state <<- "closed"
    send()
  }

  formatChannel <- function(...) {
    paste0("<Channel: ",
           outgoing$length(), " queued, ",
           awaiting$length(), " awaiting, ",
           odo, " sent>")
  }

  willSend <- FALSE
  send <- function() {
    if (getOption("async.sendLater")) {
      if (!willSend) {
        willSend <<- TRUE
        later(doSend)
      }
    } else doSend()
  }

  doSend <- function() {
    willSend <<- FALSE
    listener <- awaiting$getFirst(or=return())
    repeat {
      tryCatch({
        val <- outgoing$getFirst(
          or=switch(state,
                    "error" = {
                      state <<- "stopped"
                      listener$reject(errorValue)
                      odo <<- odo+1
                      break
                    },
                    "stopped",
                    "closed" = {
                      listener$close()
                      break
                    },
                    "running" = {
                      awaiting$prepend(listener)
                      # pass along arguments...
                      if (length(listener$args) > 0)
                        do.call(wakeup, listener$args)
                      else wakeup()
                      break
                    }))
        listener$resolve(val)
        odo <<- odo+1
      }, error=function(err) {
        warning("Unhandled channel error on send: ", err)
      })
      listener <- awaiting$getFirst(or=break)
    }
    NULL
  }

  nextThen <- function(onNext,
                       onError = function(err)
                         warning("Unhandled channel error ", err),
                       onClose, ...) {
    if (awaiting$length() > max_awaiting) stop("Channel has too many listeners")
    awaiting$append(list(resolve = onNext, reject = onError,
                         close = onClose, args = list(...)))
    send()
  }

  nextOr_ <- function(or, ...) {
    #subscribe and return a promise.
    promise(function(resolve, reject) {
      nextThen(resolve, reject, function() reject(simpleError("StopIteration")), ...)
    })
  }

  obj(emit, reject, close)
  structure(add_class(iteror(nextOr_), "channel"),
            methods=list(nextThen = nextThen, nextOr = nextOr,
                         formatChannel = formatChannel))
}

#' @exportS3Method
print.channel <- function(x, ...) {
  cat(format(x, ...), sep="\n")
}

#' @exportS3Method
format.channel <- function(x, ...) {
  attr(x, "methods")$formatChannel(...)
}

#' Receive values from channels by callback.
#'
#' `nextThen` is the callback-oriented interface to work with
#' [channel] objects. Provide the channel callback functions to
#' receive the next element, error, and closing signals; your
#' callbacks will be stored in a queue and called when values are
#' available.
#'
#' `subscribe` is similar to nextThen except that your `onNext` will be
#' called for each value the channel emits. It is just implemented
#' in terms of nextThen, with a callback that re-registers itself.
#'
#' @param x A [channel] object
#' @param onNext For [nextThen], a function to be called with the next
#'   emitted value. For [subscribe], a function to be called with each
#'   emitted value until the stream finishes.
#' @param onError Function to be called if channel stops with an
#'   error. Note that if you call nextThen multiple times to register
#'   multile callbacks, only the first will receive onError; the rest
#'   will be called with onClose.
#' @param onClose Function to be called if the channel finishes normally.
#' @param ... Undocumented.
#' @export
nextThen <- function(x, onNext, onError, onClose, ...) {
  UseMethod("nextThen")
}

#' @exportS3Method
nextThen.channel <- function(x,
                             onNext,
                             onError = function(err)
                               stop("Unhandled channel error: ", err),
                             onClose = function() NULL,
                             ...) {
  attr(x, "methods")$nextThen(onNext, onError, onClose, ...)
}

#' @export
#' @rdname channel
#' @return `is.channel(x)` returns TRUE if its argument is a channel object.
#' @param x an object.
is.channel <- function(x) {
  UseMethod("is.channel")
}

#' @exportS3Method is.channel channel
is.channel.channel <- function(x) {
  TRUE
}

#' @exportS3Method is.channel default
is.channel.default <- function(x) {
  FALSE
}

#' @export
#' @rdname nextThen
subscribe <- function(x, ...) UseMethod("subscribe")

#' @exportS3Method
subscribe.channel <- function(x, onNext, onError, onClose, ...) {
  list(onNext, onError, onClose)
  myNext <- function(value) {
    onNext(value)
    nextThen(x, myNext, onError, onClose)
  }
  nextThen(x, myNext, onError, onClose)
}

#' Combine several channels into one.
#'
#' `combine(...)` takes any number of [promise] or [channel]
#' objects. It awaits each one, and returns a [channel] object
#' which re-emits every value from its targets, in whatever
#' order they are received.
#' @param ... Each argument should be a [promise] or a [channel].
#' @return a [channel] object.
#' @author Peter Meilstrup
#' @export
combine <- function(...) {
  args <- list(...)
  channel(\(emit, reject, close) {
    remaining <- 0
    running <- FALSE
    decrement <- function(){
      remaining <<- remaining-1
      if (running && remaining == 0){
        running <<- FALSE; close()
      }
    }
    for (arg in args) {
      if (is.channel(arg)) {
        remaining <- remaining + 1
        subscribe(arg,
                  emit,
                  reject,
                  decrement)
      } else if (is.promise(arg)) {
        remaining <- remaining + 1
        then(arg,
             \(val) {emit(val); decrement()},
             reject)
      } else {
        stop("Arguments to combine() should be promises or channels")
      }
    }
    if (remaining == 0) close()
    else running <- TRUE
  })
}

# The channel method for connections wraps a connection object
# (which should be opened in non-blocking mode).
channel.connection <- function(obj, ...,
                               read = {
                                 if (summary(obj)$text == "text")
                                   c("lines", "char")
                                   else c("bin", "lines", "char")
                               },
                               read_params = {
                                 switch(read,
                                        lines = list(n = 1),
                                        char = list(nchar = 1),
                                        bin = list(what = "raw", n = 1))
                               },
                               loop = current_loop()) {
  if (!isOpen(obj, "read"))
    stop("Need to open the connection for reading before making a channel")

  read <- match.arg(read)
  readMethod <- switch(read,
                       lines=readLines,
                       char=readChar,
                       bin=readBin)

  emit <- identity
  reject <- identity
  close <- function() NULL

  arguments <- read_params |> names() |> lapply(as.name) |> structure(names=names(read_params))
  readCall <- function_(
    c(list(...), read_params),
    bquote(splice=TRUE, {
      readMethod(obj, ..(arguments), ...)
    },
    environment()
    ))

  doRead <- function(...) {
    fn <- function() {
      tryCatch({
        result <- readCall(...)
        if (length(result) == 0) {
          cat("no results...\n")
          later(fn, 1)
        } else {
          if (isIncomplete(obj)) {
            cat("incomplete results...\n")
            later(fn, 1)
          } else {
            emit(result)
          }
        }}, error=function(x) {
          close(obj);
          stop(x)
        })
    }
    fn()
  }

  channel(\(emit, reject, close) {
    emit <<- emit; reject <<- reject; close <<- close
  }, wakeup = doRead)

}
