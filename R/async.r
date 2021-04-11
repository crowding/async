# Create an asynchronous task from sequential code.
#
# `async({...})`, with an expression written in its argument, allows
# that expression to be evaluated in an asynchronous, or non-blocking
# manner. `async` returns an object with class c("async", "promise") which
# implements the [promise] interface.
#
# When an `async` object is activated, it will evaluate its expression
# until it reaches the keyword `await`. The delay object will return
# to its caller and preserve the partial state of its evaluation.
# When the awaited value is resolved, evaluation continues from where
# it last left off.
#
# When an async block completes evaluation and returns a value, the
# promise resolves with the resulting value. If the async expression
# stops with an error, the promise is rejected with that error.
#
# Note that resolution of an async, while asynchronous, happens in the
# same thread which requests the value -- there is no forking or
# parallel processing involved. It is a bit more like cooperative
# multitasking, where `await` pauses the current task and
# switches to another one.
#
# The syntax rules for a delay are analogous to that for [gen()];
# wherever an 'await' appears, its surrounding syntax must have a CPS
# implementation available.
#
# @param expr An expression, to be evaluated asynchronously on demand.
# @return A [promises::promise] object.
async <- function(expr, ...) { expr <- arg(expr)
  do(make_async,
     cps_translate(expr, async_endpoints),
     dots(...))
  expr$resolve()
}

#' @export
#' @rdname gen
await <- function(expr) {
  stop("Await must be called inside of an async() block")
}

await_cps <- function(expr) { force(expr)
  stop_ <- NULL
  await_ <- NULL
  cont_ <- NULL

  got_prom <- function(val) {
    tryCatch(p <- as.promise(val), on.error=stop)
    trace("await got promise")
    await_(prom, cont, stop) # and don't return or stop
    trace("registered with promise")
  }

  function(cont, ..., await, stop) {
    trace("await called")
    if (is_missing(await)) stop("await called, but we don't seem to be in an async()")
    stop_ <<- stop
    await_ <<- await
    cont_ <<- cont
    expr(got_prom, ..., await=await, stop=stop)
  }
}

make_async <- function(expr, ..., await=await_, return=return_, stop=stop_) {
  list(expr, ...)

  nonce <- (function() function() NULL)()
  cont <- nonce
  value <- nonce
  err <- nonce
  awaiting <- nonce
  resolve <- nonce
  reject <- nonce
  pump <- nonce

  await_ <- function(promise, cont, stop) {
    awaiting <<- promise
    cont <<- cont
    then(promise, success, failure)
  }

  success <- function(val) {
    awaiting <<- nonce

    # we have to put a value INTO pump...
    pump(cont, val)
  }

  failure <- function(val) {
    pump()
  }

  return_ <- function(val) {
    # guess we are done (roll on on.exit...)
    cat("return hit! resolving our promise\n")
    resolve(val)
  }

  stop_ <- function(err) {
    cat("Stop hit!\n")
    reject(err)
  }

  # Pump will go until we hit the first await
  promise(function(resolve, reject) {
    resolve <<- resolve
    reject <<- reject
    pump <<- make_pump(..., await=await, return=return_, stop=stop_)
    pump(expr, return)
  })
}

#' @export
print.async <- function(x, ...) {
  code <- substitute(expr, environment(x$nextElem))
  # pending nseval 0.4.1
  # scope <- nseval::caller(environment(delay$nextElem), ifnotfound="original scope not available")
  cat("Async object with code:\n", deparse(code), "\n")
}
