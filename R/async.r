#' Create an asynchronous task from sequential code.
#'
#' `async({...})`, with an expression written in its argument, allows
#' that expression to be evaluated in an asynchronous, or non-blocking
#' manner. `async` returns an object with class `c("async", "promise")` which
#' implements the [promise][promises::promise] interface.
#'
#' An example Shiny app using `async/await` is on Github:
#' [`https://github.com/crowding/cranwhales-await`](https://github.com/crowding/cranwhales-await)
#'
#' When an `async` object is activated, it will evaluate its expression
#' until it reaches the keyword `await`. The `async` object will return
#' to its caller and preserve the partial state of its evaluation.
#' When the awaited promise is resolved, evaluation continues from where
#' the `async` left off.
#'
#' When an async block finishes (either by reaching the end, or using
#' `return()`), the promise resolves with the resulting value. If the
#' async block stops with an error, the promise is rejected with
#' that error.
#'
#' Async blocks and generators are conceptually related and share much
#' of the same underlying mechanism. You can think of one as "output"
#' and the other as "input". A generator pauses until a value is
#' requested, runs until it has a value to output, then pauses again.
#' An async runs until it requires an external value, pauses until
#' it receives the value, then continues.
#'
#' The syntax rules for an `async` are analogous to those for [gen()];
#' `await` must appear only within the arguments of functions for
#' which there is a pausable implementation (See `[pausables()]`). For
#' `async` the default `split_pipes=TRUE` is enabled; this will
#' rearrange some expressions to satisfy this requirement.
#'
#' When `split_pipes=FALSE`, `await()` can only appear in the
#' arguments of [pausables] and not ordinary R functions.  This is an
#' inconvenience as it prevents using `await()` in a pipeline. With
#' `split_pipes=TRUE` applies some syntactic sugar: if an `await()`
#' appears in the leftmost, unnamed, argument of an R function, the
#' pipe will be "split" at that call using a temporary variable. For
#' instance, either
#'
#'     async(makeRequest() |> await() |> sort())
#'
#' or, equivalently,
#'
#'     async(sort(await(makeRequest())))
#'
#' will be effectively rewritten to something like
#'
#'     async({.tmp <- await(makeRequest()); sort(.tmp)})
#'
#' This works only so long as `await` appears in calls that evaluate
#' their leftmost arguments normally. `split_pipes` can backfire if
#' the outer call has other side effects; for instance
#' `suppressWarnings(await(x))` will be rewritten as `{.tmp <-
#' await(x); suppressWarnings(x)}`, which would defeat the purpose.
#'
#' If `async` is given a function expression, like `async(function(...)
#' ...)`, it will return an "async function" i.e. a function that
#' constructs an async.
#'
#' @param expr An expression, to be executed asynchronously.
#' @param trace Enable verbose logging by passing a function to
#'   `trace`, like `async(trace=cat, {...})`. `trace` should take a
#'   character argument. Helper `with_prefix` makes a function that
#'   prints a message with the given prefix. You can also say something
#'   like `trace=browser` for "single stepping" through an async.
#' @param split_pipes Rewrite chained calls that use `await`
#'   (see below)
#' @param compileLevel Compilation level; same options as for [gen].
#'
#' @param ... Undocumented.
#' @param prefix A function to print debugging messages. `trace=cat`
#'   will log async actions to the console; `trace=with_prefix("myPrefix")`
#'   adds a prefix if you have more than one async to debug.
#' @return `async()` returns an object with class "promise," as
#'   defined by the [promises] package (i.e., rather than the kind of
#'   promise used in R's lazy evaluation.)
#'
#' @examples
#' myAsync <- async(for (i in 1:4) {
#'   await(delay(5))
#'   cat(i, "\n")
#' }, trace=with_prefix("myAsync"))
#'
#' @export
async <- function(expr, ..., split_pipes=TRUE, trace=trace_,
                  compileLevel=options$compileLevel) {
  expr_ <- arg(expr)
  expr <- NULL
  if (identical(expr(expr_)[[1]], quote(`function`))) {
    defn <- coroutine_function(expr_,
                               quote(async::async),
                               ...,
                               split_pipes=split_pipes,
                               compileLevel=compileLevel)
    return(value(defn))
  }
  list(trace, split_pipes, compileLevel)
  .contextName <- "wrapper"
  envir <- env(expr_)
  translated_ <- cps_translate(expr_, async_endpoints, split_pipes=split_pipes)
  args <- c(translated_, orig=forced_quo(expr(expr_)), trace=quo(trace), dots(...))
  set_dots(environment(), args)
  make_async(..., callingEnv=env(expr_), compileLevel=compileLevel)
}

#' @export
#' @rdname async
#'
#' @param prom A promise, or something that can be converted to such
#'   by [promises::as.promise()].
#' @return In the context of an `async`, `await(x)` returns the
#'   resolved value of a promise `x`, or stops with an error.
await <- function(prom) {
  stop("Await called outside of async")
}

await_cps <- function(.contextName, prom) { force(prom)
  function(cont, ..., pause, await, stp, trace) {
    if (missing(await)) stop("await used, but this is not an async")
    list(cont, await, pause, stp, trace)
    promis <- NULL
    success <- NULL
    value <- NULL
    named(then <- function() {
      trace("await: resolve\n")
      if(success) cont(value) else stp(value)
    })
    named(await_ <- function(val) {
      val <- promises::as.promise(val)
      promis <<- val
      trace("await: got promise\n")
      success <<- NULL
      await(then,
            promis,
            function(val) {success <<- TRUE; promis <<- NULL; value <<- val},
            function(err) {success <<- FALSE; promis <<- NULL; value <<- err})
    })
    prom(await_, ..., pause=pause, await=await, stp=stp, trace=trace)
  }
}

#' @import promises
make_async <- function(expr, orig = expr, ...,
                       compileLevel = 0,
                       trace = trace_,
                       local = TRUE,
                       callingEnv,
                       targetEnv = if (local) new.env(parent=callingEnv) else callingEnv,
                       debugR, debugInternal) {
  list(orig, expr, ..., trace)
  .contextName <- "async"
  pause <- NULL

  nonce <- (function() function() NULL)()
  state <- "pending" #print method uses this
  value <- nonce
  resolve_ <- NULL
  reject_ <- NULL

  named(getState <- function() state)

  named(return_ <- function(val) {
    trace("async: return (resolving)\n")
    state <<- "resolved"
    value <<- val
    resolve_(val) # avoid gathering this as a tailcall
    val
  })

  named(stop_ <- function(val) {
    trace("async: stop (rejecting)\n")
    value <<- val
    state <<- "rejected"
    reject_(val)
    val
  })

  globalNamed(replace <- function(resolve, reject) {
    resolve_ <<- resolve
    reject_ <<- reject
  })

  await_ <- NULL
  awaitNext_ <- NULL
  #await_ and awaitNext_
  eval(await_handlers)

  pr <- add_class(promise(function(resolve, reject) {
    resolve_ <<- resolve
    reject_ <<- reject
  }), "async", "coroutine")

  pump <- make_pump(expr, ...,
                    rtn=return_, stp=stop_, await=await_,
                    awaitNext=awaitNext_, trace=trace,
                    targetEnv=targetEnv)

  pause <- environment(pump)$pause_
  bounce <- environment(pump)$bounce_

  pr$orig <- orig
  pr$state <- environment()
  if (compileLevel != 0) {
    pr <- compile(pr, level=compileLevel)
  }
  debugAsync(pr, R=debugR, internal=debugInternal)
  pr$state$pump()
  pr
}

# shared with both async and stream
await_handlers <- quote({
  awaiting <- nonce
  await_state <- "xxx"

  named(check_wake <- function()
    switch(await_state,
           "awaiting"={
             trace("await: got callback while still running\n")
             # pump is still running, stream.r::awaitNext_cps will be
             # watching for this and simply not pause
             await_state <<- "xxx"
           },
           "awaited"={
             trace("await: waking up\n")
             await_state <<- "xxx"
             pump()
           }
           ))

  named(await_ <- function(cont, promise, success, failure) {
    list(promise, success, failure)

    succ <- function(val) {
      trace("await: success\n")
      awaiting <<- NULL
      success(val)
      check_wake()
    }
    fail <- function(val) {
      trace("await: fail\n")
      awaiting <<- NULL
      failure(val)
      check_wake()
    }
    awaiting <<- promise
    await_state <<- "awaiting"
    promises::then(promise, succ, fail)
    trace("await: registered\n")
    await_state <<- "awaited"
    if (is.null(awaiting))
      bounce(cont) else pause(cont)
  })

  named(awaitNext_ <- function(cont, strm, success, error, finish) {
    list(strm, success, error, finish)
    succ <- function(val) {
      trace("awaitNext: got value\n")
      awaiting <<- NULL
      success(val)
      check_wake()
    }
    err <- function(val) {
      trace("awaitNext: stream error")
      awaiting <<- NULL
      error(val)
      check_wake()
    }
    fin <- function() {
      trace("awaitNext: stream finished\n")
      awaiting <<- NULL
      finish()
      check_wake()
    }
    await_state <<- "awaiting"
    awaiting <<- strm
    nextThen(strm, succ, err, fin)
    await_state <<- "awaited"
    if (is.null(awaiting))
      bounce(cont) else pause(cont)
  })
})

#' @exportS3Method
getPump.async <- function(x, ...) x$state$pump
#' @exportS3Method
getOrig.async <- function(x, ...) x$orig
#' @exportS3Method

#' @exportS3Method
#' @rdname format
#' @return `getState(a)` on an [async] might return "pending", "resolved" or
#' "rejected".
getState.async <- function(x, ...) x$state$getState()

#' @exportS3Method
getStartSet.async <- function(x, ...) {
  c(NextMethod(),
    list(
      replace = x$state$replace,
      getState = x$state$getState
))
}

#' @exportS3Method
reconstitute.async <- function(orig, munged) {
  list(orig, munged)
  pr <- add_class(promise(function(resolve, reject) {
    # assign "resolve_" and "reject_" callbacks in the base function...
    then(orig, \(val){stop("Result went to the wrong promise!")},
         \(err){stop("Error went to the wrong promise!")})
    munged$replace(resolve, reject)
  }), "async", "coroutine")
  pr$orig <- orig$orig
  pr$state <- munged
  pr
}
