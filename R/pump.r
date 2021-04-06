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
      #trace(deparse(list(...)))
      trace(where <- "pump ret forced")
      cont <<- function() {
        trace(where <- "Thunk called")
        #trace(deparse(list(cont, ...)))
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
  }

  got_value <- function(val) {
    trace(where <- "pump got value")
    value <<- val
  }

  pump <- function(new_cont) {
    if (missing(new_cont)) {
      result <- reset(cont, cont <<- nonce)(got_value, ..., ret=ret, stop=stop)
    } else {
      # the continuation given to us here should be one
      # that the yield/await handler exfiltrated,
      # so context should already be established
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



make_generator <- function(expr, ...) { list(expr, ...)
  nonce <- function() NULL
  cont <- nonce
  yielded <- nonce

  yield <- function(cont, val) {
    trace("Yield handler called")
    cont <<- function() cont(val) # yield() returns its input
    yielded <<- val
    val
  }

  pump <- make_pump(expr, ..., yield=yield)

  nextElem <- function(...) {
    trace("nextElem")
    result <- tryCatch(
      if (identical(cont, nonce)) pump() else pump(reset(cont, cont <<- nonce)),
      error = function(e) {
        trace("nextElem pump threw error")
        if (identical(conditionMessage(e), 'StopIteration'))
          e else stop(e)
      })
    if (identical(cont, nonce)) {
      trace("nextElem reached end")
      cont <<- function() stop("StopIteration")
      if (identical(yielded, nonce)) {
        stop("StopIteration")
      } else {
        trace("nextElem returning value yielded from end")
        reset(yielded, yielded <<- nonce)
      }
    } else if (identical(yielded, nonce)) {
      warning("nextElem: neither yielded nor finished")
    } else {
      trace("nextElem returning yielded value")
      reset(yielded, yielded <<- nonce)
    }
  }

  add_class(itertools::new_iterator(nextElem), "generator")
}

#' @export
print.generator <- function(gen) {
  code <- substitute(expr, environment(gen$nextElem))
  cat("Generator object with code:\n", deparse(code), "\n")
  # the really spiffo thing would be if you could propagate srcrefs
  # through the syntactic transform, then introspect back to which code
  # corresponds to the current "state" of the run. (i.e. which "yield"
  # we are paused at.) You could poke around at the "cont"
  # argument. and figure out which "yield" it corresponds to? It would
  # also be spiffo to support inline substitutions in the original
  # source comments.
}
