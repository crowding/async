
#' Create an iterator using sequential code.
#'
#' `gen({...})` with an expression written in its argument, creates a
#' generator, which can be thought of as a block of code whose
#' execution can pause and resume. From the inside, a generator looks
#' like you are writing sequential code with loops, branches and such,
#' writing values to the outside world by calling `yield()`. From the
#' outside, a generator behaves like an iterator over
#' an indefinite collection.
#'
#' Generators are not based on forking or parallel OS processes; they
#' run in the same thread as their caller. The control flow in a
#' generator is interleaved with that of the R code which queries it.
#'
#' When `nextItem` is called on a generator, the generator evaluates
#' its given expression until it reaches a call to `yield(...).` The
#' value passed to `yield` is returned. The generator's execution
#' state is preserved and will continue form where ti left off on the
#' next call to `nextItem.`
#'
#' There are some syntactic restrictions on what you can write in a
#' generator expression. `yield` must appear only directly within
#' control flow operators. Wherever `yield` appears in a generator
#' expression, the calls it is nested within must have CPS
#' implementations. (This package provides CPS implementations for
#' several base R control flow builtins; the list is in the
#' non-exported variable `generators:::cps_builtins`).
#' @export
gen <- function(expr, ...) { expr <- arg(expr)
  do(make_generator,
     cps_translate(expr,
                   endpoints=gen_endpoints),
     dots(...))
}

#' @export
#' @rdname gen
yield <- function(expr) {
  stop("Yield must be called inside of a gen() block")
}

yield_cps <- function(expr) { force(expr)
  function(cont, ..., ret, yield) {
    trace("Yield called")
    if (is_missing(yield)) stop("yield called, but we do not seem to be in a generator")
    got_val <- function(val) {
      trace("Got a yield value")
      ret(yield, cont, val)
    }
    expr(got_val, ..., ret=ret, yield=yield)
  }
}

make_generator <- function(expr, ...) { list(expr, ...)

  nonce <- function() NULL
  cont <- nonce
  yielded <- nonce

  yield <- function(cont, val) {
    trace("Yield handler called")
    cont <<- function(...) cont(val) # yield() returns its input
    yielded <<- val
    val
  }

  # "expr" represents the syntax tree, and is a constructor
  # that returns the entry continuation.
  # "make_pump" 
  pump <- make_pump(expr, ..., yield=yield)

  nextElem <- function(...) {
    trace("nextElem")
    result <- tryCatch(if (identical(cont, nonce)) {
      trace("nextElem: starting from scratch")
      pump()
    } else {
      trace("nextElem has continuation")
      pump(reset(cont, cont <<- nonce))
    },
    error = function(e) {
      trace("nextElem pump threw error: ", deparse(e))
      cont <<- function(...) stop("StopIteration")
      #if (identical(conditionMessage(e), 'StopIteration'))
      #  e else stop(e)
      stop(e)
    })
    if (identical(cont, nonce)) {
      trace("nextElem reached end")
      cont <<- function(...) stop("StopIteration")
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
print.generator <- function(x) {
  code <- substitute(expr, environment(x$nextElem))
  cat("Generator object with code:\n", deparse(code), "\n")
  # the really spiffo thing would be if you could propagate srcrefs
  # through the syntactic transform, then introspect back to which code
  # corresponds to the current "state" of the run. (i.e. which "yield"
  # we are paused at.) You could poke around at the "cont"
  # argument. and figure out which "yield" it corresponds to? It would
  # also be spiffo to support inline substitutions in the original
  # source comments.
}
