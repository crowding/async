#' A slightly different way to construct a [channel], compared to
#' [channel.function].
#' This kind of channel is "lazy", its callback is called
#'
#' This might make it easier to pass arguments along to 
#' But it can also pass arguments to nextThen
#' (which is good for socket connections...)
#'
#' @param `fun` should be a function which takes three arguments
#'   `\(emit, reject, close)`. This function will be called for _each_
#'   requested value; Once `fun` is called, it should later call one
#'   of its arguments `emit(val)`, `reject(err)`, or `close()`, and
#'   that will yield the value.
#' @param max_awaiting Maximum number of queued-up requests before
#' throwing an error.
lazy_channel <- function(fun, ..., max_awaiting=500) {

  stop_unused(...)
  check_channel_function_args(fun)

  # list of awaiting callbacks
  awaiting <- deque()
  state <- "sleeping"
  errorValue <- NULL
  odo <- 0

  summaryChannel <- function(...) {
    stop_unused(...)
    list(
      state = state,
      outgoing = 0,
      awaiting = awaiting$length(),
      sent = odo)
  }

  dummy <- function(x) stop("that was alraedy resolved")

  handleNext <- function() {
    # will be a closure calling startHandler...
    awaiting$getFirst(or=return(invisible(NULL)))()
  }

  startHandler <- function(onEmit, onReject, onClose, ...) {
    list(onEmit, onReject, onClose, ...)
    state <<- "awaiting"
    reset <- function() {
      onEmit <<- dummy; onReject <<- dummy; onClose <<- dummy
      later(handleNext)
    }

    doEmit <- function(val) {
      if (state != "awaiting")
        stop(" channel is ", state)
      callback <- onEmit
      state <<- "sleeping"
      reset()
      later(\(){callback(val); odo <<- odo+1})
    }

    doError <- function(...) {
      if (state != "awaiting")
        stop("Error requested but channel is ", state)
      callback <- onReject
      state <<- "error"
      if (inherits(..1, "condition")) {
        errorValue <<- ..1
        if (nargs() > 1L)
          warning("additional arguments ignored in reject()")
      } else {
        errorValue <<- simpleError(...)
      }
      reset()
      # later() confuses "returning a simpleError object" with actually
      # throwing an error, thus the NULL
      later(\() {callback(errorValue); invisible(NULL)})
    }

    doClose <- function() {
      if (state != "awaiting") {
        stop("Closing requested but channel is ", state)
      }
      callback <- onClose
      state <<- "closed"
      reset()
      later(callback)
    }

    fun(doEmit, doError, doClose, ...)
  }

  nextThen <- function(onEmit,
                       onReject = function(err)
                         warning("Unhandled channel error ", err),
                       onClose = function(err)
                         warning("Unhandled channel closing"),
                       ...) {
    list(onEmit, onReject, onClose, ...)

    len <- awaiting$length()
    switch(
      state,
      sleeping = startHandler(onEmit, onReject, onClose, ...),
      awaiting = {
        if (len >= max_awaiting) {
          stop("Channel has too many outstanding requests")
        } else {
          awaiting$append(
            \() startHandler(onEmit, onReject, onClose, ...))
        }
      },
      # default
      later(onClose)
    )
  }

  nextOr_ <- function(or, ...) {
    #subscribe and return a promise.
    promise(function(resolve, reject) {
      nextThen(resolve,
               reject,
               function() reject(simpleError("StopIteration")),
               ...)
    })
  }

  structure(add_class(iteror(nextOr_), c("lazy_channel",
                                         "channel")),
            methods=list(nextThen = nextThen,
                         nextOr = nextOr,
                         summaryChannel = summaryChannel,
                         close = close))

}

#' @exportS3Method
format.lazy_channel <- function(x, ...) {
  s <- attr(x, "methods")$summaryChannel(...)
  c(paste0("<Channel (", s$state, "): ",
           s$awaiting, " awaiting, ",
           s$sent, " sent>"))
}

#' @exportS3Method
summary.lazy_channel <- function(object, ...) {
  c(attr(object, "methods")$summaryChannel(...))
}
