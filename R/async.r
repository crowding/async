#' Create an asynchronous task from sequential code.
#'
#' `async({...})`, with an expression written in its argument, allows
#' that expression to be evaluated in an asynchronous, or non-blocking
#' manner. `async` returns an object with class c("async", "promise") which
#' implements the [promise] interface.
#'
#' When an `async` object is activated, it will evaluate its expression
#' until it reaches the keyword `await`. The delay object will return
#' to its caller and preserve the partial state of its evaluation.
#' When the awaited value is resolved, evaluation continues from where
#' it last left off.
#'
#' When an async block completes evaluation and returns a value, the
#' promise resolves with the resulting value. If the async expression
#' stops with an error, the promise is rejected with that error.
#'
#' The syntax rules for a delay are analogous to that for [gen()];
#' 'await' must appear only within the arguments of functions for which
#' there is a restartable implementation.
#'
#' Async blocks and generators are conceptually related and share much
#' of the same underlying mechanism. You can think of one as "output"
#' and the other as "input". A generator pauses until a value is
#' requested, runs until it has a value to output, then pauses again.
#' An async runs until it requires an external value, then pauses until
#' it receives the value.
#'
#' @param expr An expression, to be evaluated asynchronously on demand.
#' @return `async` constructs and returns a [promises::promise] object.
#' @export
async <- function(expr, ...) { expr <- arg(expr)
  do(make_async,
     cps_translate(expr, async_endpoints),
     orig=forced_quo(expr),
     dots(...))
}

#' @export
#' @rdname async
#'
#' @param prom A promise, or something that can be converted to such
#'   by [promises::as.promise()].
#' @return In the context of an `async`, `await` returns the value of
#'   a promise, or stops with an error
await <- function(prom) {
  stop("Await called outside of async")
}

await_cps <- function(prom) { force(prom)
  function(cont, ..., await, pause, stop) {
    if (is_missing(await)) stop("await called, but we don't seem to be in an async()")
    list(cont, await, stop)

    trace("await called")
    and_then <- function(branch, val) {
      # will receive either a branch to continue or a branch to error
      trace("await taking promise branch")
      branch(val)
    }
    got_prom <- function(val) {
      tryCatch(prom <- as.promise(val), on.error=stop)
      trace("await got promise")
      pause(and_then)
      await(prom, cont, stop)
    }
    prom(got_prom, ..., pause=pause, await=await, stop=stop)
  }
}

#' @import promises
make_async <- function(expr, orig=arg(expr), ...) {
  list(expr, orig, ...)

  nonce <- (function() function() NULL)()
  state <- "pending"
  awaiting <- nonce
  value <- nonce
  err <- nonce

  return_ <- function(val) {
    trace("async resolving")
    state <<- "resolved"
    value <<- val
    resolve_(val)
  }

  stop_ <- function(val) {
    trace("async rejecting")
    err <<- val
    state <<- "rejected"
    reject_(val)
  }

  await_ <- function(promise, success, failure) {
    list(promise, success, failure)
    awaiting <<- promise
    succ <- function(val) {
      pump(success, val)
    }
    fail <- function(err) {
      pump(failure, err)
    }
    then(promise, succ, fail)
    trace("await registered with promise")
  }

  resolve_ <- NULL
  reject_ <- NULL

  pr <- add_class(promise(function(resolve, reject) {
    resolve_ <<- resolve
    reject_ <<- reject
  }), "async")

  pump <- make_pump(expr, ..., return=return_, stop=stop_, await=await_)
  pump()
  pr$orig <- orig
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
