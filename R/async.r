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
#' @param expr An expression, to be executed asynchronously.
#' @param trace Enable verbose logging by passing a function to
#'   `trace`, like `async(trace=cat, {...})`. `trace` should take a
#'   character argument. Helper `with_prefix` makes a function that
#'   prints a message with the given prefix. You can also say something
#'   like `trace=browser` for "single stepping" through an async.
#' @param split_pipes Rewrite chained calls that use `await`
#'   (see below)
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
                  compileLevel=get("compileLevel", parent.env(environment()))) {
  expr_ <- arg(expr)
  force(trace)
  translated_ <- cps_translate(expr_, async_endpoints, split_pipes=split_pipes)
  args <- c(translated_, orig=forced_quo(expr_), trace=quo(trace), dots(...))
  set_dots(environment(), args)
  make_async(..., compileLevel=compileLevel)
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
  function(cont, ..., await, pause, stop, trace) {
    if (missing(await)) base::stop("await used, but this is not an async")
    list(cont, await, pause, stop, trace)
    promis <- NULL
    success <- NULL
    value <- NULL
    then <- function() {
      trace("await: resolve\n")
      if(success) cont(value) else stop(value)
    }
    awaited <- function() {
      pause(then)
    }
    await_ <- function(val) {
      val <- promises::as.promise(val)
      promis <<- val
      if(verbose) trace("await: got promise\n")
      success <<- NULL
      await(awaited,
            promis,
            function(val) {success <<- TRUE; promis <<- NULL; value <<- val},
            function(err) {success <<- FALSE; promis <<- NULL; value <<- err})
    }
    prom(await_, ..., pause=pause, await=await, stop=stop, trace=trace)
  }
}

#' @import promises
make_async <- function(expr, orig=arg(expr), ..., compileLevel=0, trace=trace_) {
  list(expr, orig, ..., trace)
  .contextName <- "async"

  nonce <- (function() function() NULL)()
  state <- "pending" #print method uses this
  awaiting <- nonce
  value <- nonce
  err <- nonce
  resolve_ <- NULL
  reject_ <- NULL

  getState <- function() state

  resolve <- function(val) {
    trace("async: return (resolving)\n")
    state <<- "resolved"
    value <<- val
    resolve_(val) # avoid gathering this as a tailcall
    val
  }

  reject <- function(val) {
    trace("async: stop (rejecting)\n")
    err <<- val
    state <<- "rejected"
    reject_(val)
    val
  }

  replace <- function(resolve, reject) {
    resolve_ <<- resolve
    reject_ <<- reject
  }

  await_ <- function(cont, promise, success, failure, ...) {
    list(promise, success, failure)
    succ <- function(val) {
      trace("await: success\n")
      success(val)
      pump()
    }
    fail <- function(val) {
      trace("await: fail\n")
      failure(val)
      pump()
    }
    awaiting <<- promise
    promises::then(promise, succ, fail)
    trace("await: registered\n")
    cont(...)
  }

  pr <- add_class(promise(function(resolve, reject) {
    resolve_ <<- resolve
    reject_ <<- reject
  }), "async")

  pump <- make_pump(expr, ...,
                    return=resolve, stop=reject, await=await_, trace=trace)
  pr$orig <- orig
  pr$state <- environment()
  if (compileLevel != 0) {
    pr <- compile(pr, level=compileLevel)
  }
  pr$state$pump()
  pr
}

#' @exportS3Method
getEntry.async <- function(x) environment(x$state$pump)$entry
#' @exportS3Method
getReturn.async <- function(x) x$state$resolve
#' @exportS3Method
getStop.async <- function(x) x$state$reject
#' @exportS3Method
getCurrent.async <- function(x) environment(x$state$pump)$cont
#' @exportS3Method
getOrig.async <- function(x) x$orig
#' @exportS3Method
getStartSet.async <- function(x) {
  list(entry=getEntry(x),
       resolve=getReturn(x),
       reject=getStop(x),
       replace=x$state$replace,
       pump=get("pump", envir=x$state),
       runPump=environment(get("pump", (x$state)))$runPump,
       getState=get("getState", x$state))
}

#' @export
print.async <- function(x, ...) {
  cat(format(x, ...), sep="\n")
}

#' @export
format.async <- function(x, ...) {
  if (is.null(x$orig)) {
    a <- deparse(call("async(...)"))
  }
  a <- deparse(call("async", expr(x$orig)), backtick = TRUE)
  b <- format(env(x$orig))
  c <- NextMethod(x)
  c(a, b, c)
}

#' @export
getState.async <- function(x) x$state$getState()

compile.async <- function(x, level) {
  if (abs(level) >= 1) {
    munged <- munge( x )
    munged$orig <- x$orig
    if (abs(level) >= 3) {
      stop("TODO: Aggressive inlining")
    } else if (abs(level) >= 2) {
      stop("TODO: Basic inlining/constant folding")
    }
    # create a new promise with this
    if (level <= -1) {
      pr <- add_class(promise(function(resolve, reject) {
        # assign "resolve_" and "reject_" callbacks in the base function...
        then(x, \(val){stop("Result went to the wrong promise!")},
                \(err){stop("Error went to the wrong promise!")})
        munged$replace(resolve, reject)
      }), "async")
      pr$orig <- x$orig
      pr$state <- munged
      if (paranoid) {
        expect_properly_munged(x, pr)
      }
      pr
    } else if (level >= 1) {
      stop("TODO: code generation")
    }
  } else x
}
