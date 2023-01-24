#` @export
stream <- function(expr, ..., split_pipes=TRUE, lazy=TRUE, trace=trace_,
                   compileLevel=get("compileLevel", parent.env(environment()))) {
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
  strm <- make_stream(..., lazy=lazy, targetEnv=new.env(parent=envir),
                      compileLevel=compileLevel)
  #if (compileLevel != 0) str <- compile(strm, compileLevel)
  strm
}

make_stream <- function(expr, orig=expr, ...,
                        trace=trace_, targetEnv, compileLevel, lazy) {
  list(orig, expr, ..., trace, targetEnv, compileLevel, lazy)
  .contextName <- "stream"

  nonce <- (function() function() NULL)()
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

  getState %<-% function() state

  return_ %<-% function(val) {
    trace("stream: return\n")
    state <<- "resolved"
    value <<- val
    close_(val) # avoid gathering this as a tailcall
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
    doEmit(val)
    if (!lazy || state == "woken") {
      trace("stream: continuing\n")
      state <<- "running"
      bounce_val(cont, val)
    } else {
      trace("stream: pausing\n")
      state <<- "yielded"  # what does "cont" point to here?
      pause_val(cont, val)
    }
  }

  eval(await_handlers) # await_ and awaitNext_; see async.r
  #defines "awaiting" and makes updates to "state"

  doClose %<-% function(val) close_()
  doReject %<-% function(val) reject_(val)
  doEmit %<-% function(val) emit_(val)

  pump <- make_pump(expr, ...,
                    rtn=doClose, stp=doReject, await=await_,
                    awaitNext=awaitNext_, yield=yield_,
                    trace=trace, targetEnv=targetEnv)

  bounce_val <- environment(pump)$bounce_val_
  bounce <- environment(pump)$bounce_
  pause_val <- environment(pump)$pause_val_
  pause <- environment(pump)$pause_

  wakeup <- function(x) {
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
  if (compileLevel != 0) {
    ch <- compile(ch, level=compileLevel)
  }
  if (lazy) {
    state <- "yielded"
  } else {
    state <- "running"
    ch$state$pump()
  }
  ch
}


#' @rdname format
#' @exportS3Method
format.stream <- function(x, ...) {
  envir <- environment(x$state$pump)
  code <- getOrig(x)
  a <- deparse(call("async", code), backtick=TRUE)
  b <- format(envir, ...)
  state <- getState(x)
  cont <- getNode(x)
  c <- paste0(c("<stream [",
                state,
                " at `", cont, "`",
                if (state=="stopped")
                  c(": ", capture.output(print(envir$err))),
                "]>"), collapse="")
  d <- NextMethod()
  c(a, b, c, d)
}

#' @exportS3Method
#' @rdname format
getNode.stream <- function(x, ...) {
  environment(x$state$pump)$getCont()
}

#' @export
#' @rdname format
getState.stream <- function(x) x$state$getState()

#' @exportS3Method
debugAsync.stream <- function(x, R=current$R, internal=current$internal) {
  set <- environment(get("pump", x$state))$setDebug
  current <- set()
  set(R, internal)
}

#' @exportS3Method
print.stream <- function(x, ...) {
  cat(format(x, ...), sep="\n")
}

#' @exportS3Method
#' @rdname format
getOrig.stream <- function(x, ...) x$orig

#' Wait for the next value from a channel or stream.
#'
#' `awaitNext` can be used within an [async] or [stream] coroutine.
#' When reached, the routine will recister to receive the next element
#' from an async or a coroutine object.
#'
#' @param strm A [channel] or [stream] object.
#' @param or This argument will be evaluated and returned in the case
#'   the channel closes. If not specified, awaiting on a closed stream
#'   will throw an error with message "StopIteration".
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
    traceBinding("state")
    state <<- "xxx"
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
