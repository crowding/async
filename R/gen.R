
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
#' its given expression until it reaches a call to `yield(...).` The
#' value passed to `yield` is returned. The generator's execution
#' state is preserved and will continue from where if left off on the
#' next call to `nextElem.`
#'
#' The generator expression is evaluated in a local environment.
#'
#' Generators are not based on forking or parallel OS processes; they
#' run in the same thread as their caller. The control flow in a
#' generator is interleaved with that of the R code which queries it.
#'
#' A generator expression can use any R functions, but a call to
#' "yield" may only appear in some positions. This package contains
#' "restartable" equivalents to R's base control flow functions, such as
#' `if`, `while`, `try`, `{}`, `||` and so on.  A call to `yield` may
#' appear only on the arguments of these restartable functions. So
#' this random walk generator:
#' ```
#' rwalk <- gen({x <- 0; repeat {x <- yield(x + rnorm(1))}})
#' ```
#' is legal, because `yield` appears within arguments to `{}`,
#' `repeat`, and `<-`, for which this package has interruptible
#' definitions. However, this:
#' ```
#' rwalk <- gen({x <- rnorm(1); repeat {x <- rnorm(1) + yield(x)}})
#' ```
#' is not legal, because `yield` appears in an argument to `+`, which
#' does not have a restartable definition.
#' @export
gen <- function(expr, ...) { expr <- arg(expr)
  do(make_generator,
     cps_translate(expr,
                   endpoints=gen_endpoints),
     orig=forced_quo(expr),
     dots(...))
}

#' @export
#' @rdname gen
yield <- function(expr) {
  stop("Yield called outside a generator")
}

yield_cps <- function(expr) { force(expr)
  function(cont, ..., ret, pause, yield) {
    if (is_missing(yield)) stop("yield in code but this is not a generator")
    list(cont, ret, pause, yield)
    got_val <- function(val) {
      force(val)
      trace("Yield called")
      pause(function() cont(val))
      yield(val)
    }
    expr(got_val, ..., ret=ret, pause=pause, yield=yield)
  }
}

make_generator <- function(expr, orig=NULL, ...) { list(expr, ...)

  nonce <- (function () function() NULL)()
  yielded <- nonce
  err <- nonce
  state <- "paused"

  return_ <- function(val) {
    force(val)
    trace(where <- "generator got return value")
    state <<- "finished"
  }

  stop_ <- function(val) {
    trace("generator got stop")
    err <<- val
    state <<- "stopped"
  }

  yield_ <- function(val) {
    force(val)
    trace("Yield handler called")
    state <<- "paused"
    yielded <<- val
  }

  # yield handler goes into a wrapper to gain access to "pause"
  pump <- make_pump(expr, ..., return=return_, stop=stop_, yield=yield_)

  nextElem <- function(...) {
    trace("nextElem")
    switch(state,
           stopped =,
           finished = stop("StopIteration"),
           paused = {
             state <<- "running"
             pump()
           },
           running = stop("Generator is already running (recursive loop?)"))
    switch(state,
           stopped = stop(err),
           finished = stop("StopIteration"),
           paused = {
             if(identical(yielded, nonce)) stop("generator paused but no value yielded")
             else reset(yielded, yielded <<- nonce)
           },
           running = stop("Generator paused without yielding"))
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
