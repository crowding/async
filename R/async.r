#' Create an asynchronous task from sequential code.
#'
#' `async({...})`, with an expression written in its argument, allows
#' that expression to be evaluated in an asynchronous, or non-blocking
#' manner. `async` returns an object with class `c("async", "promise")` which
#' implements the [promises::promise] interface.
#'
#' When an `async` object is activated, it will evaluate its expression
#' until it reaches the keyword `await`. The delay object will return
#' to its caller and preserve the partial state of its evaluation.
#' When the awaited value is resolved, evaluation continues from where
#' the `async` left off.
#'
#' When an async block finishes (either by reaching the end, or using
#' `return()`), the promise resolves with the resulting value. If the
#' async block stops with an error, the promise is rejected with
#' that error.
#'
#' The syntax rules for an `async` are analogous to those for [gen()];
#' `await` must appear only within the arguments of functions for
#' which there is a pausable implementation (See `?[pausable]`). By
#' default `split_pipes` is enabled and this will reorder some
#' expressions to satisfy this requirement.
#'
#' Async blocks and generators are conceptually related and share much
#' of the same underlying mechanism. You can think of one as "output"
#' and the other as "input". A generator pauses until a value is
#' requested, runs until it has a value to output, then pauses again.
#' An async runs until it requires an external value, pauses until
#' it receives the value, then continues.
#'
#' When `split_pipes=FALSE`, `await()` can only appear in the
#' arguments of [pausable] functions and not ordinary R functions.
#' This is a inconvenience as it prevents using `await()` in a
#' pipeline. `async` by default has `split_pipes=TRUE` which enables
#' some syntactic sugar: if an `await()` appears in the leftmost,
#' unnamed, argument of an R function, the pipe will be "split" at
#' that call using a temporary variable. For instance `sort(await(x))`
#' will be silently rewritten as `{..async.tmp <- await(x);
#' sort(..async.tmp)}`. This works only so long as `await` appears in
#' calls that evaluate their leftmost arguments
#' normally. `split_pipes` can backfire if the outer call has other
#' side effects; for instance `suppressWarnings(await(x))` will be
#' rewritten as `{.tmp <- await(x); suppressWarnings(x)}`, which
#' defeats the purpose.
#'
#' @param expr An expression, to be executed asynchronously.
#' @param split_pipes Silently rewrite chained calls that use `await` (see below)
#'
#' @return `async` constructs and returns a [promises::promise]
#'   object.
#' @export
async <- function(expr, ..., split_pipes=TRUE) { expr <- arg(expr)
  do(make_async,
     cps_translate(expr, async_endpoints, split_pipes=split_pipes),
     orig=forced_quo(expr),
     dots(...))
}

#' @export
#' @rdname async
#'
#' @param prom A promise, or something that can be converted to such
#'   by [promises::as.promise()].
#' @return In the context of an `async`, `await` returns the resolved of
#'   a promise, or stops with an error.
await <- function(prom) {
  stop("Await called outside of async")
}

await_cps <- function(prom) { force(prom)
  function(cont, ..., await, pause, stop, trace) {
    if (is_missing(await)) base::stop("await used, but this is not an async")
    list(cont, ..., await, pause, stop, trace)

    and_then <- function(branch, val) {
      # will receive either a branch to continue or a branch to error
      trace("await: taking promise branch\n")
      branch(val)
    }
    got_prom <- function(val) {
      tryCatch(prom <- as.promise(val), on.error=stop)
      trace("await: got promise\n")
      pause(and_then)
      await(prom, cont, stop) # return either "cont" or "stop" to and_then
    }
    prom(got_prom, ..., pause=pause, await=await, stop=stop, trace=trace)
  }
}

#' @import promises
make_async <- function(expr, orig=arg(expr), ..., trace=trace_) {
  list(expr, orig, ..., trace)

  nonce <- (function() function() NULL)()
  state <- "pending" #print method uses this
  awaiting <- nonce
  value <- nonce
  err <- nonce

  return_ <- function(val) {
    trace("async: return (resolving)\n")
    state <<- "resolved"
    value <<- val
    resolve_(val)
  }

  stop_ <- function(val) {
    trace("async: stop (rejecting)\n")
    err <<- val
    state <<- "rejected"
    reject_(val)
  }

  await_ <- function(promise, success, failure) {
    list(promise, success, failure)
    awaiting <<- promise
    succ <- function(val) {
      trace("await: success\n")
      pump(success, val)
    }
    fail <- function(err) {
      trace("await: fail\n")
      pump(failure, err)
    }
    then(promise, succ, fail)
    trace("await: registered\n")
  }

  resolve_ <- NULL
  reject_ <- NULL

  pr <- add_class(promise(function(resolve, reject) {
    resolve_ <<- resolve
    reject_ <<- reject
  }), "async")

  pump <- make_pump(expr, ..., return=return_, stop=stop_, await=await_, trace=trace)
  pump()
  pr$orig <- orig
  pr$state <- environment()
  pr
}

#' @export
print.async <- function(x, ...) {
  code <- substitute(expr, environment(x$nextElem))
  # pending nseval 0.4.1
  # scope <- nseval::caller(environment(delay$nextElem), ifnotfound="original scope not available")
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
