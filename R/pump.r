reset <- function(...) list(...)[[1]] #evaluates all args then returns first.

add_class <- function(x, ...) {
  attr(x, "class") <- c(..., attr(x, "class"))
  x
}

pump <- function(expr) make_pump(expr)()

make_pump <- function(expr, ...,
                      ret=base::stop("unused"),
                      stop=base::stop("unused"),
                      eliminate.tailcalls = TRUE) { list(expr, ...)
  nonce <- function() NULL # a sigil value
  cont <- expr
  value <- nonce

  on_finish <- function(...) {
    trace("pump finishing with value")
    value
  }

  if (eliminate.tailcalls) {
    ret <- function(cont, ...) {
      trace(where <- "pump ret")
      list(cont, ...) #force
      trace(where <- "pump ret forced")
      cont <<- function() {
        trace(where <- "Thunk called")
        cont(...)
      }
    }
  } else {
    ret <- function(cont, ...) {
      trace(where <- "pump fake ret")
      list(cont, ...) #force
      cont(...)
    }
  }

  stop <- function(...) {
    trace(where <- "pump stop")
    trace(deparse(list(...)))
    on_finish <<- function() {
      trace("pump finishing with error")
      #trace(deparse(list(...)))
      base::stop(...)
    }
    cont <<- function() {
      base::stop("StopIteration")
    }
  }

  got_value <- function(val) {
    trace(where <- "pump got value")
    value <<- val
  }

  pump <- function(new_cont) {
    if (missing(new_cont)) {
      trace("pump: No starting continuation given, using stored")
      result <- reset(cont, cont <<- nonce)(got_value, ..., ret=ret, stop=stop)
    } else {
      # the continuation given to us here should be one
      # that the yield/await handler exfiltrated,
      # so context should already be established
      trace("pump: given starting continuation")
      result <- new_cont()
    }
    trace(where <- "pump first return")
    while(!identical(cont, nonce)) {
      trace(where <- "pump thunk")
      (reset(cont, cont <<- nonce))()
    }
    trace(where <- "pump finish")
    cont <<- on_finish
    on_finish()
  }
  pump #reset(pump, debug(pump))
}




