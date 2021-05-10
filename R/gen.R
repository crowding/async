
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
#' built-in (pausables), equivalents to R's base control flow
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
#' @export
gen <- function(expr, ..., split_pipes=FALSE, trace=trace_) { expr <- arg(expr)
  do(make_generator,
     cps_translate(expr,
                   endpoints=gen_endpoints,
                   split_pipes=split_pipes),
     orig=forced_quo(expr),
     trace=arg(trace),
     dots(...))
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
    got_val <- function(val) {
      force(val)
      if(verbose) trace("yield\n")
      pause(function() cont(val))
      yield(val)
    }
    expr(got_val, ..., ret=ret, pause=pause, yield=yield, trace=trace)
  }
}

make_generator <- function(expr, orig=NULL, ..., trace=trace_) { list(expr, ...)

  nonce <- (function () function() NULL)()
  yielded <- nonce
  err <- nonce
  state <- "paused"

  return_ <- function(val) {
    force(val)
    trace("generator: return\n")
    state <<- "finished"
  }

  stop_ <- function(val) {
    trace("generator: stop\n")
    err <<- val
    state <<- "stopped"
  }

  yield_ <- function(val) {
    trace("generator: yield\n")
    yielded <<- val
    state <<- "paused"
  }

  # yield handler goes into a wrapper to gain access to "pause"
  pump <- make_pump(expr, ..., return=return_, stop=stop_, yield=yield_, trace=trace)

  nextElem <- function(...) {
    trace("generator: nextElem\n")
    switch(state,
           stopped =,
           finished = stop("StopIteration"),
           running = stop("Generator is already running (recursive loop?)"),
           paused = {
             state <<- "running"
             pump()
             assert(state != "running",
                    msg="Generator paused without yielding?")
             switch(state,
                    stopped = stop(err),
                    finished = stop("StopIteration"),
                    paused = {
                      assert(!identical(yielded, nonce),
                             msg="generator paused but no value yielded?")
                      reset(yielded, yielded <<- nonce)
                    })
           })
  }

  g <- add_class(itertools::new_iterator(nextElem), "generator")
  g$orig <- orig
  g
}

#' @export
print.generator <- function(x, ...) {
  cat(format(x, ...), sep="\n")
}

#' @export
format.generator <- function(x, ...) {
  envir <- environment(x$nextElem)
  code <- envir$orig
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
