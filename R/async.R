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
#'   `trace`, like `trace=cat`. This function should take a character
#'   argument.
#' @param split_pipes Rewrite chained calls that use `await` (see
#'   below)
#' @param compileLevel Compilation level; same options as for [gen].
#' @param ... Undocumented.
#' @param debugR Set TRUE to enter the browser immediately on
#'   executing the first R expression.
#' @param debugInternal Set TRUE to single-step at implementation
#'   level, immediately upon execution.
#'
#' @return `async()` returns an object with class "promise," as
#'   defined by the [promises] package (i.e., rather than the kind of
#'   promise used in R's lazy evaluation.)
#'
#' @examples
#' myAsync <- async(for (i in 1:4) {
#'   await(delay(5))
#'   cat(i, "\n")
#' })
#'
#' @export
async <- function(expr, ..., split_pipes=TRUE,
                  compileLevel=getOption("async.compileLevel"),
                  debugR=FALSE, debugInternal=FALSE,
                  trace=getOption("async.verbose")) {
  expr_ <- arg(expr)
  expr <- NULL
  if (identical(expr(expr_)[[1]], quote(`function`))) {
    defn <- coroutine_function(expr_,
                               quote(async::async),
                               ...,
                               split_pipes=split_pipes,
                               compileLevel=compileLevel,
                               debugR=debugR,
                               debugInternal=debugInternal,
                               trace=trace)
    return(value(defn))
  }
  list(split_pipes, compileLevel, trace)
  .contextName <- "wrapper"
  envir <- env(expr_)
  translated_ <- cps_translate(expr_, async_endpoints, split_pipes=split_pipes)
  args <- c(translated_, orig=forced_quo(expr(expr_)), trace=quo(trace), dots(...))
  set_dots(environment(), args)
  as <- make_async(..., callingEnv=env(expr_), compileLevel=compileLevel)
  debugAsync(as, R=debugR, internal=debugInternal, trace=trace)
  as
}

#' @export
#' @rdname async
#'
#' @param prom A promise, or something that can be converted to such
#'   by [promises::as.promise()].
#' @param error This argument will be forced if the promise rejects.  If
#'   it is a function, it will be called with the error condition.
#' @return In the context of an `async` or `stream`, `await(x)` returns
#'   the resolved value of a promise `x`, or stops with an error.
await <- function(prom, error) {
  stop("Await called outside of async")
}

await_cps <- function(.contextName, prom, error) {
  list(prom, maybe(error))
  function(cont, ..., pause, await, stp) {
    list(cont, pause, maybe(await), stp)
    if (missing(await)) stop("await used, but this is not an async")
    promis <- NULL
    success <- NA
    value <- NULL

    node(gotErrorFn <- function(val) {
      success <<- NA
      if (is.function(val)) {
        val <- val(value)
      }
      cont(val)
    })
    if (is_missing(error)
        || is_R(error) && identical(R_expr(error), missing_value())) {
      node(error <- function() stp(value))
    } else {
      error <- error(gotErrorFn, ...,
                 await=await, pause=pause, stp=stp)
    }
    node(then <- function() {
      if(success) cont(value) else error()
    })
    node(await_ <- function(val) {
      val <- promises::as.promise(val)
      promis <<- val
      success <<- NULL
      await(then,
            promis,
            function(val) {success <<- TRUE; promis <<- NULL; value <<- val},
            function(val) {success <<- FALSE; promis <<- NULL; value <<- val})
    })
    prom(await_, ..., pause=pause, await=await, stp=stp)
  }
}

#' @import promises
make_async <- function(expr, orig = expr, ...,
                       compileLevel = 0,
                       local = TRUE,
                       callingEnv,
                       trace = identity, #FIXME
                       targetEnv = if (local) new.env(parent=callingEnv) else callingEnv,
                       debugR, debugInternal) {
  list(orig, expr, ...)
  .contextName <- "async"
  pause <- NULL

  nonce <- (function() function() NULL)()
  state <- "pending" #print method uses this
  value <- nonce
  resolve_ <- NULL
  reject_ <- NULL

  node(getState <- function() state)

  node(return_ <- function(val) {
    state <<- "resolved"
    value <<- val
    resolve_(val) # avoid gathering this as a tailcall
    val
  })

  node(stop_ <- function(val) {
    value <<- val
    state <<- "rejected"
    reject_(val)
    val
  })

  globalNode(replace <- function(resolve, reject) {
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
                    awaitNext=awaitNext_,
                    targetEnv=targetEnv)

  pause <- environment(pump)$pause_
  bounce <- environment(pump)$bounce_

  pr$orig <- orig
  pr$state <- environment()
  if (compileLevel != 0) {
    pr <- compile(pr, level=compileLevel)
  }
  debugAsync(pr, R=debugR, internal=debugInternal, trace=trace)
  pr$state$pump()
  pr
}

# shared with both async and stream
await_handlers <- quote({
  awaiting <- nonce
  await_state <- "xxx"

  node(check_wake <- function()
    switch(await_state,
           "awaiting"={
             # pump is still running, stream.r::awaitNext_cps will be
             # watching for this and simply not pause
             await_state <<- "xxx"
           },
           "awaited"={
             await_state <<- "xxx"
             pump()
           }
           ))

  node(await_ <- function(cont, promise, success, failure) {
    list(promise, success, failure)

    succ <- function(val) {
      awaiting <<- NULL
      success(val)
      check_wake()
    }
    fail <- function(val) {
      awaiting <<- NULL
      failure(val)
      check_wake()
    }
    awaiting <<- promise
    await_state <<- "awaiting"
    promises::then(promise, succ, fail)
    await_state <<- "awaited"
    if (is.null(awaiting))
      bounce(cont) else pause(cont)
  })

  node(awaitNext_ <- function(cont, strm, success, error, finish) {
    list(strm, success, error, finish)
    succ <- function(val) {
      awaiting <<- NULL
      success(val)
      check_wake()
    }
    err <- function(val) {
      awaiting <<- NULL
      error(val)
      check_wake()
    }
    fin <- function() {
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
getStartSet.async <- function(x, ...) {
  c(NextMethod(),
    list(
      replace = x$state$replace,
      getState = x$state$getState
))
}

#' @rdname format
#' @description `summary(a)$state` of an [async] might be "pending", "resolved" or
#' "rejected".
#' @exportS3Method
summary.async <- function(object, ...) {
  c(list(code=object$orig,
         state=object$state$getState(),
         node=environment(object$state$pump)$getCont(),
         envir=environment(object$state$pump)$targetEnv),
    NextMethod("summary"))
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
