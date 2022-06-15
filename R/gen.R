
#' Create an iterator using sequential code.
#'
#' `gen({...})` with an expression written in its argument, creates a
#' generator, which acts like a block of code whose execution can
#' pause and resume. From the "inside," a generator looks like you are
#' writing sequential code with loops, branches and such, writing
#' values to the outside world by calling `yield()`. From the "outside,"
#' a generator behaves like an iterator over an indefinite collection.
#'
#' When `nextElem` is called on a generator, the generator executes
#' its given expression until it reaches a call to `yield(...).`
#' `nextElem` returns argument to `yield` is returne, and the
#' generator's execution state is preserved. The generator will resume
#' on the next call to `nextElem()`.
#'
#' The generator expression is evaluated in a local environment.
#'
#' Generators are not based on forking or parallel OS processes; they
#' run in the same thread as their caller. The control flow in a
#' generator is interleaved with that of the R code which queries it.
#'
#' A generator expression can use any R functions, but a call to
#' `yield` may only appear in some positions. This package has several
#' built-in [pausables], equivalents to R's base control flow
#' functions, such as `if`, `while`, `tryCatch`, `<-`, `{}`, `||` and
#' so on.  A call to `yield` may only appear in an argument of one of
#' these pausable functions. So this random walk generator:
#'
#' ```r
#' rwalk <- gen({x <- 0; repeat {x <- yield(x + rnorm(1))}})
#' ```
#' is legal, because `yield` appears within arguments to `{}`,
#' `repeat`, and `<-`, for which this package has pausable
#' definitions. However, this:
#' ```r
#' rwalk <- gen({x <- rnorm(1); repeat {x <- rnorm(1) + yield(x)}})
#' ```
#' is not legal, because `yield` appears in an argument to `+`, which
#' does not have a pausable definition.
#'
#' @param expr An expression, to be turned into an iterator.
#' @param ... Undocumented.
#' @param split_pipes Silently rewrite expressions where "yield"
#'   appears in chained calls. See [async].
#' @param trace Optional tracing function for debugging. See [async].
#' @return An object with class "[iterator][iterators-package]".
#' @export
gen <- function(expr, ..., split_pipes=FALSE, trace=trace_) { expr <- arg(expr)
  args_ <- c(cps_translate(expr,
                           endpoints=gen_endpoints,
                           split_pipes=split_pipes),
             orig=forced_quo(expr),
             trace=arg(trace),
             dots(...))
  set_dots(environment(), args_)
  gen <- make_generator(...)
  gen
}

#' @export
#' @rdname gen
yield <- function(expr) {
  stop("Yield called outside a generator")
}

yield_cps <- function(expr) { force(expr)
  function(cont, ..., ret, pause, yield, trace) {
    if (missing_(arg(yield))) base::stop("yield used but this is not a generator")
    list(cont, ret, pause, yield, trace)
    `yield_` <- function(val) {
      force(val)
      trace("yield\n")
      yield(val, cont, val)
    }
    expr(yield_, ..., ret=ret, pause=pause, yield=yield, trace=trace)
  }
}

make_generator <- function(expr, orig=arg(expr), ..., trace=trace_) { list(expr, ...)
  nextElemOr_ <- NULL

  gen_cps <- function(expr) { force(expr)
    function(cont, ..., stop, return, pause, trace) {
      list(stop, return, pause, trace)
      nonce <- sigil()
      yielded <- nonce
      err <- nonce
      state <- "yielded"

      return_ <- function(val) {
        force(val)
        trace("generator: return\n")
        state <<- "finished"
        return(val)
      }

      stop_ <- function(val) {
        trace("generator: stop\n")
        err <<- val
        state <<- "stopped"
        stop(val)
      }

      yield_ <- function(val, cont, ...) {
        trace("generator: yield\n")
        yielded <<- val
        state <<- "yielded"
        pause(cont, ...)
      }

      nextElemOr_ <- function(or, ...) {
        trace("generator: nextElemOr\n")
        nextElem <- switch(state,
               stopped =,
               finished = or,
               running = stop("Generator already running (or finished unexpectedly?)"),
               yielded = {
                 state <<- "running"
                 on.exit({
                   if (state == "running") {
                     state <<- "finished"
                     #just a warning  (or not) bc we're probably on our
                     #way out with a more detailed error
                     # warning("Generator finished unexpectedly")
                   }
                 })
                 pump()
                 switch(state,
                        running = {
                          state <<- "finished"
                          stop("Generator finished unexpectedly")
                        },
                        stopped = stop(err),
                        finished = or,
                        yielded = {
                          assert(!identical(yielded, nonce),
                                 msg="generator yielded but no value?")
                          tmp <- yielded
                          yielded <<- nonce
                          tmp
                        },
                        stop("Generator in an unknown state"))
               },
               stop("Generator in an unknown state"))
        nextElem # no tailcalls
      }
      nextElemOr_ <<- nextElemOr_
      expr(return_, ..., stop=stop_, return=return_, yield=yield_,
           pause=pause, trace=trace)
    }
  }

  pump <- make_pump(gen_cps(expr), trace=trace)
  g <- add_class(iteror(nextElemOr_), "generator")
  g
}

# Code for walking over an async/generator and gathering information about its
# nodes and graphs.

#' @export
getEntry <- function(x) UseMethod("getEntry")
#' @export
getReturn <- function(x) UseMethod("getReturn")
#' @export
getStop <- function(x) UseMethod("getStop")
#' @export
getCurrent <- function(x) UseMethod("getStop")
#' @export
getOrig <- function(x) UseMethod("getOrig")
#' @export
getStartSet <- function(x) UseMethod("getStartSet")

#' @exportS3Method
getEntry.generator <- function(x)
  environment(get("pump", envir=environment(x$nextElemOr)))$entry
#' @exportS3Method
getReturn.generator <- function(x)
  environment(get("pump", envir=environment(x$nextElemOr)))$return_
#' @exportS3Method
getStop.generator <- function(x)
  environment(get("pump", envir=environment(x$nextElemOr)))$stop_
#' @exportS3Method
getCurrent.generator <- function(x)
  environment(get("pump", envir=environment(x$nextElemOr)))$cont
#' @exportS3Method
getOrig.generator <- function(x)
  expr(get("orig", envir=environment(x$nextElemOr)))
getStartSet.generator <- function(x) {
  list(START=getEntry(x),
       STOP=getStop(x),
       RETURN=getReturn(x),
       pump=get("pump", envir=environment(x$nextElemOr)),
       nextElemOr=x$nextElemOr)
}

#' @export
print.generator <- function(x, ...) {
  cat(format(x, ...), sep="\n")
}

#' @export
format.generator <- function(x, ...) {
  envir <- environment(x$nextElemOr)
  code <- get("orig", envir)
  a <- deparse(call("gen", expr(code)), backtick=TRUE)
  b <- format(env(code), ...)
  state <- envir$state
  c <- paste0(c("<Generator [",
         if(state=="stopped")
           c("stopped: ", capture.output(print(envir$err)))
         else state,
         "]>"), collapse="")
  c(a, b, c)
}

