#` @export
#' Create an asynchronous iterator by writing sequential code.
#'
#' (Experimental as of async 0.4.) `stream(...)` constructs a [channel]
#' object, i.e. an asynchronous iterator, which will compute and
#' return values according to sequential code written in `expr`. A
#' `stream` is a coroutine wearing a [channel] interface in the same
#' way that `async` is a coroutine wearing a [promise] interface, and a
#' [gen] is a coroutine sitting behind an [iteror] interface.
#'
#' In a stream expression, you can call `yield()` to emit a value, and
#' `await()` to wait for a value from a [promise]. To have your stream
#' wait for values from another stream or [channel], call
#' `awaitNext()`; you can also use `awaitNext` when you are writing an
#' `async`. You can also use a simple `for` loop to consume all future
#' values from a stream or channel.
#'
#' The lower-level interface to consume values from a stream is by using
#' [nextThen] from the [channel] interface.
#'
#' Streams come in both "lazy" and "eager" varieties. If `lazy=TRUE`,
#' a stream starts idle, and does not process anything
#' until it is woken up by a call to its channel's `nextThen`. It will
#' pause after reaching `yield` if there are no more outstanding
#' requests. If `lazy=FALSE`, a stream will begin executing
#' immediately, not pausing on `yield`, possibly queuing up emitted
#' values until it needs to `await` something.
#'
#' (For comparison, in this package, [gen] are lazy in that they do
#' not start executing until a call to `nextElemOr` and pause
#' immediately after `yield`, while [async] blocks are eager,
#' starting at construction and running until they hit an `await`.)
#'
#' @examples
#'
#' # emit values _no more than_ once per second
#' count_to <- function(n, interval=1) {
#'   list(n, interval) #force
#'   stream({
#'     for (i in 1:n) {await(delay(interval)); yield(i)}
#'   })
#' }
#'
#' accumulate <- function(in) { force(in)
#'   stream({
#'     sum <- 0;
#'     for (i in in) {sum <- sum + i; yield(sum)}
#'   })
#' }
#'
#' @param expr A coroutine expression, using some combination of
#'   `yield`, `await`, `awaitNext`, `yieldFrom`, standard control flow
#'   operators and other calls.
#' @param ... Undocumented.
#' @param split_pipes See description under [async]; defaults to
#'   `TRUE`.
#' @param lazy If true, pause at start and after yield() if there are no
#'   listeners.
#' @param trace An optional tracing function.
#' @param compileLevel Compilation level.
#' @param debugR Set TRUE to single-step debug at R level.  call.
#' @param debugInternal Set TRUE to single-step debug at coroutine
#'   implementation level. Use [debugAsync()] to enable or disable
#'   debugging on a stream after it has been created.
#' @return An object with (at least) classes "stream", "channel",
#'   "coroutine", "iteror".
#' @author Peter Meilstrup
stream <- function(expr, ..., split_pipes=TRUE, lazy=TRUE, trace=trace_,
                   compileLevel=get("compileLevel", parent.env(environment())),
                   debugR=FALSE, debugInternal=FALSE) {
  envir <- arg_env(expr)
  expr <- arg(expr)
  .contextName <- "wrapper"
  args_ <- c(cps_translate(expr,
                           endpoints=stream_endpoints,
                           split_pipes=split_pipes),
             orig=forced_quo(expr),
             trace=arg(trace),
             dots(...))
  set_dots(environment(), args_)
  strm <- make_stream(..., lazy=lazy,
                      compileLevel=compileLevel,
                      targetEnv=new.env(parent=envir))
  debugAsync(strm, R=debugR, internal=debugInternal)
  strm
}

make_stream <- function(expr, orig=expr, ...,
                        trace=trace_, targetEnv, compileLevel, lazy) {
  list(orig, expr, ..., trace, targetEnv, compileLevel, lazy)
  .contextName <- "stream"

  nonce <- (function() function() NULL)()
  if (verbose) traceBinding("state", "xxx")
  state <- "xxx" #print method uses this
  awaiting <- nonce
  yielded <- nonce
  value <- nonce
  err <- nonce
  emit_ <- NULL
  reject_ <- NULL
  close_ <- NULL

  bounce <- NULL
  pause_val <- NULL
  pause <- NULL

  getState %<g-% function() state

  return_ %<-% function(val) {
    trace("stream: return\n")
    state <<- "resolved"
    value <<- val
    close_() # avoid gathering this as a tailcall
    val
  }

  stop_ %<-% function(val) {
    trace("stream: reject\n")
    err <<- val
    state <<- "rejected"
    reject_(val)
    val
  }

  yield_ %<-% function(cont, val) {
    yielded <<- val
    state <<- "yielding"
    emit_(val)
    if (!lazy || state == "woken") {
      trace("stream: continuing\n")
      state <<- "running"
      bounce_val(cont, val)
    } else {
      trace("stream: pausing\n")
      state <<- "yielded"
      pause_val(cont, val)
    }
  }

  eval(await_handlers) # await_ and awaitNext_; see async.r
  #defines "awaiting" and makes updates to "state"

  pump <- make_pump(expr, ...,
                    rtn=return_, stp=stop_, await=await_,
                    awaitNext=awaitNext_, yield=yield_,
                    trace=trace, targetEnv=targetEnv)

  bounce_val <- environment(pump)$bounce_val_
  bounce <- environment(pump)$bounce_
  pause_val <- environment(pump)$pause_val_
  pause <- environment(pump)$pause_

  wakeup %<g-% function(x) {
    trace("stream: wakeup\n")
    switch(state,
           "yielding" = {state <<- "woken"},
           "yielded" = {state <<- "running"; pump()}
           )
  }

  replace %<-% function(emit, reject, close) {
    emit_ <<- emit
    reject_ <<- reject
    close_ <<- close
  }

  ch <- add_class(channel(replace, wakeup=wakeup), "stream", "coroutine")

  ch$orig <- orig
  ch$state <- environment()
  ch$wakeup <- wakeup
  if (lazy) {
    state <- "yielded"
  } else {
    state <- "running"
  }
  tmp <- lazy # bc "lazy" will get moved in compilation if destructive=TRUE
  if (compileLevel != 0) {
    ch <- compile(ch, level=compileLevel)
  }
  if (!tmp) ch$state$pump()
  ch
}

#' @exportS3Method
reconstitute.stream <- function(orig, munged) {
  st <- add_class(channel(munged$replace, wakeup=munged$wakeup),
                  "stream", "coroutine")
  st$orig <- orig$orig
  st$state <- munged
  st$wakeup <- munged$wakeup
  st
}


#' @exportS3Method
getNode.stream <- function(x, ...) {
  environment(x$state$pump)$getCont()
}

#' @exportS3Method
getState.stream <- function(x) x$state$getState()
#' @exportS3Method
getOrig.stream <- function(x, ...) x$orig
#' @exportS3Method
getPump.stream <- function(x, ...) x$state$pump
#' @exportS3Method
getReturn.stream <- function(x, ...) x$state$return_
#' @exportS3Method
getStop.stream <- function(x, ...) x$state$stop_

#' @exportS3Method
getStartSet.stream <- function(x,...) {
  c(NextMethod(), list(
    replace = x$state$replace,
    getState = x$state$getState,
    wakeup = x$state$wakeup))
}

#' Wait for the next value from a channel or stream.
#'
#' `awaitNext` can be used within an [async] or [stream] coroutine.
#' When reached, `awaitNext` will register to receive the next element
#' from an async or a coroutine object.
#'
#' @param strm A [channel] or [stream] object.
#' @param or This argument will be evaluated and returned in the case
#'   the channel closes. If not specified, awaiting on a closed stream
#'   will raise an error with message "StopIteration".
#' @param error Provide a function here to handle an error.
#' @return In the context of an `async` or `stream`, `awaitNext(x)`
#'   returns the resolved value of a promise `x`, or stops with an
#'   error.
#' @export
awaitNext <- function(strm, or, error) {
  stop("awaitNext called outside of async")
}

awaitNext_cps <- function(.contextName,
                          strm,
                          or,
                          error) {
  list(.contextName, strm, maybe(or), maybe(error))

  function(cont, ..., awaitNext, stp, trace) {
    if (missing(awaitNext)) stop("awaitNext used, but this is not an async")
    list(cont, awaitNext, stp, trace)

    nonce <- function(x) NULL
    state <- "xxx"
    value <- NULL
    awaiting <- FALSE

    gotError %<-% function(val) {
      state <<- "xxx"
      if (is.function(val)) {
        val <- val(result)
      }
      cont(val)
    }

    if (is_missing(error)
        || is_R(error) && missing_(R_expr(error))) {
      error %<-% function(val) stp(result)
    } else {
      error <- error(gotError, ...,
                       awaitNext=awaitNext, stp=stp, trace=trace)
    }
    if (is_missing(or)
        || is_R(error) && missing_(R_expr(error))) {
      or %<-% function() stp("StopIteration")
    } else {
      or <- or(cont, ...,
               awaitNext=awaitNext, stp=stp, trace=trace)
    }

    then %<-% function() {
      trace("awaitNext: resolve\n")
      switch(state,
             "success" = {state <<- "xxx"; cont(value)},
             "error" = {state <<- "xxx"; error(value)},
             "closed" = {state <<- "xxx"; or()},
             stp(paste0("awaitNext: unexpected state ", state)))
    }
    await_ %<-% function(val) {
      trace("awaitNext: got channel\n")
      state <<- "xxx"
      value <<- NULL
      awaiting <<- TRUE
      awaitNext(then, val,
                function(val) {state <<- "success"; value <<- val; awaiting <<- FALSE},
                function(err) {state <<- "error"; value <<- err; awaiting <<- FALSE},
                function() {state <<- "closed"; value <<- NULL; awaiting <<- FALSE})
    }
    strm <- strm(await_, ..., await=await,
                 awaitNext=awaitNext, stp=stp, trace=trace)
  }
}
