# Particular channels and channel functions

#' Apply a function to each element of a channel
#'
#' `ch_apply(ch, fun)` constructs a channel which applies `fun` to each element
#' received from `ch`.
#' @param ch A [channel] object.
#' @return A new [channel] object.
#' @author Peter Meilstrup
#' @export
ch_apply <- function(ch, fun, ...) {
  ch <- channel(ch)
  lazy_channel(
    function(emit, reject, finish) {
      nextThen(ch, \(val) emit(fun(val)), reject, finish)
    })
}

#' Combine several channels or promises into one channel.
#'
#' `ch_combine(...)` takes any number of [promise] or [channel]
#' objects. It awaits each one, and returns a [channel] object
#' which re-emits every value from its targets, in whatever
#' order they are received.
#' @param ... Each argument should be a [promise] or a [channel].
#' @return a [channel] object.
#' @author Peter Meilstrup
#' @export
ch_combine <- function(...) {
  # HMM. This is "eager" as opposed to "lazy"?
  args <- list(...)
  channel(\(emit, reject, finish) {
    remaining <- 0
    running <- FALSE
    decrement <- function(){
      remaining <<- remaining-1
      if (running && remaining == 0){
        running <<- FALSE;
        finish()
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
        stop("Arguments to ch_combine() should be promises or channels")
      }
    }
    if (remaining == 0) finish()
    else running <- TRUE
  })
}
