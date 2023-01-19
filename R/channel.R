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
                 nextElemOr=getFirst,
                 getFirst=getFirst,
                 getLast=getLast,
                 length=getLength),
            class=c("deque", "funiteror", "iteror", "iter"))
}


#' Create an object representing a sequence of future values.
#'
#' A `channel` is an object with an interface that represents
#' a sequence of values yet to be determined. It is something like a
#' combination of a [promise] and an [iteror].
#'
#' It may be used to represent and work with data coming in over a
#' connection, data values being logged over time, a queue of incoming
#' requests, and things of that nature.
#'
#' The friendly way to create a channel and use them in asynchronous
#' programming is to use a [stream] coroutine. Inside of `stream()`,
#' use `await` to wait on promises, `awaitNext` to wait on other
#' streams and `yield(val)` to yield values. To signal end of iteration
#' use `return()` (which will discard its value) and to signal an
#' error use `stop()`.
#'
#' The friendly way to consume values from a channel is to use
#' `[awaitNext(ch)]` within an `async` or `stream` construct.
#'
#' The low-level interface to request values from a channel is to call
#' `nextThen(ch, onNext, onError, onFinished)`, providing callback
#' functions for at least `onNext(val)`. Those callbacks will be
#' appended to an internal queue, and will be called as soon as data
#' is available, in the order that requests were received.
#'
#' You can also treat a channel as an iterator over promises, calling
#' `nextElem(pri)` to return a [promise] representing the next
#' available value. Each promise created this way will be resolved in
#' the order that data come in. Note that this way there is no special
#' signal for end of iteration; a promise will reject with
#' the sigil value `"StopIteration"` to signal end of iteration.
#'
#' Be careful with the iterator-over-promises interface though: if you
#' call `as.list.iteror(pr)` you may get stuck in an infinite loop, as
#' `as.list` keeps calling `nextElem` and receives more promises to
#' represent values that exist only hypothetically. This is the main
#' reason for the `max_listeners` limit.
#'
#' The low-level interface to create a channel object is to call
#' `channel(function(emit, reject, cancel) {...})`, providing your own
#' function in its argument; your function will receive those three
#' methods as arguments. Then use whatever means to arrange to call
#' `emit(val)` some time in the future as data comes in. When you are
#' done emitting values, call the `finish()` callback. To report an
#' error use the `reject(err)` callback. The next requestor will
#' receive the error. If there is more than one listener, other
#' queued listeners will get a `finish` signal.
#'
#' @param impl A user-provided function; it will receive three
#'   callback functions as arguments, in order, `emit(val)`,
#'   `reject(err)` and `finish()`
#' @param max_queue The maximum number of outgoing values to store if
#'   there are no listeners. Beyond this, calling `emit` will return
#'   an error.
#' @param max_awaiting The maximum number of pending requests. If
#'   there are this many outstanding requests, for values, calling
#'   `nextThen(ch, ...)` or `nextElem(ch)` will raise an error.
#' @param wakeup Optional; provide a callback when there the outgoing data
#'   queue is empty and there are still listeners.
#' @return a channel object, supporting methods "nextThen" and "nextElem"
#'
#' @author Peter Meilstrup
channel <- function(impl, max_queue=500L, max_awaiting=500L,
                    wakeup=function() NULL) {
  # list of callbacks waiting to be made having yet to be sent
  # each is a list(resolve=, reject=, finish=
  outgoing <- deque()
  # list of values waiting for a callback
  awaiting <- deque()
  state <- "running"
  errorvalue <- NULL
  odo <- 0

  emit <- function(val) {
    if (state != "running")
      stop("Can not emit; state is ", state)
    if (outgoing$length() >= max_queue)
      stop("Outgoing queue is full!")
    outgoing$append(val)
    send()
    val
  }

  reject <- function(err) {
    state <<- "error"
    errorvalue <<- err
    send()
  }

  finish <- function() {
    state <<- "finished"
    send()
  }

  formatChannel <- function(...) {
    paste0("<Channel ",
           outgoing$getLength(), " queued, ",
           awaiting$getLength(), " awaiting, ",
           odo, " sent>")
  }

  send <- function() {
    listener <- awaiting$getFirst(or=return())
    repeat {
      tryCatch(
        switch(state,
               "error" = {
                 state <<- "stopped"
                 listener$reject(errorvalue)
                 odo <<- odo+1
               },
               "stopped",
               "finished" = listener$finish(),
               "running" = {
                 val <- outgoing$getFirst(
                   or={
                     awaiting$prepend(listener)
                     wakeup()
                     break
                   })
                 listener$resolve(val)
                 odo <<- odo+1
               }),
        error=function(err)
          warning("Unhandled channel error on resolve: ", err))
      listener <- awaiting$getFirst(or=break)
    }
  }

  nextThen <- function(onNext,
                       onError=function(err)
                         warning("Unhandled promise_iter error ", err),
                       onFinished=function() NULL) {
    if (!state %in% c("running", "error")) stop("Channel is no longer operating")
    if (awaiting$length() > max_awaiting) stop("Channel has too many listeners")
    awaiting$append(list(resolve=onNext, reject=onError, finish=onFinished))
    send()
  }

  nextElemOr <- function(or) {
    #subscribe and return a promise.
    promise(function(resolve, reject) {
      nextThen(resolve, reject, function() reject("StopIteration"))
    })
  }

  impl(emit, reject, finish)
  structure(list(nextThen=nextThen, nextElemOr=nextElemOr, formatChannel=formatChannel),
            class=c("channel", "funiteror", "iteror", "iter"))

}

#' @exportS3Method
print.channel <- function(x, ...) {
  cat(format(x, ...), sep="\n")
}

#' @exportS3Method
format.channel <- function(x, ...) {
  x$formatChannel(...)
}

#' @export
nextThen <- function(x, onNext, onError, onFinish, ...) {
  UseMethod("nextThen")
}

#' @exportS3Method
nextThen.channel <- function(x,
                             onNext,
                             onError = function(err)
                               stop("Unhandled channel error: ", err),
                             onFinish = function() NULL,
                             ...) {
  x$nextThen(onNext, onError, onFinish, ...)
}
