make_delay <- function(expr, ...) { list(expr, ...)
  nonce <- function() NULL
  cont <- nonce

  block <- function(cont) {
    cont <<- cont
  }

  resolve <- function(cont) {
  }

  pump <- make_pump(expr, ..., block=block, resolve=resolve)

  check <- function(resolve, reject) {
    result <- tryCatch({
      if (identical(cont, nonce)) {
        #first time
        pump()
      } else {
        #subsequent times
        pump(reset(cont, cont <<- nonce))
      }
      if (identical(cont, nonce)) {
        #last time
        resolve(result)
      }},
      error = function(e) reject(e))
  }

  add_class(promises::promise(check), "delay")
}

#' @export
print.delay <- function(delay) {
  code <- substitute(expr, environment(delay$nextElem))
  # pending nseval 0.4.1
  # scope <- nseval::caller(environment(delay$nextElem), ifnotfound="original scope not available")
  cat("Delay object with code:\n", deparse(code), "\n")
}

await_cps <- function(expr) { maybe(expr)
  function(cont, ..., ret, await) {
    if (missing(await)) stop("await called, but we do not seem to be in an async")

    got_val <- function(...) ret(await, cont, ...)

    if (nseval:::is_missing(expr)) {
      ret(block, cont)
    } else {
      expr(got_val, ..., ret=ret, block=block)
    }
  }
}

resolve_cps <- function(expr) { force(expr)
  function(cont, ..., ret, resolve) {
    if (missing(resolve)) stop("resolve called but we do not seem to be in a delay")
    got_val <- function(...) ret(resolve, cont, ...)
    expr(got_val, ..., ret=ret, resolve=resolve)
  }
}
