#' Create an iterator using sequential code.
#'
#' `gen({...})` with an expression written in its argument, creates a
#' generator, an object which computes an indefinite sequence.
#'
#' On the "inside", that is the point of view of code you write in
#' `{...}`, is ordinary sequential code using conditionals, branches,
#' loops and such, outputting one value after another with `yield()`.
#' For example, this code creates a generator that computes a random
#' walk:
#'
#' ```r
#' rwalk <- gen({
#'   x <- 0;
#'   repeat {
#'     x <- x + rnorm(1)
#'     yield(x)
#'   }
#' })
#' ```
#'
#' On the "outside," that is, the object returned by `gen()`, a
#' generator behaves like an iterator over an indefinite
#' collection. So we can collect the first 100 values from the above
#' generator and compute their mean:
#'
#' ```r
#' rwalk |> itertools2::take(100) |> as.numeric() |> mean()
#' ```
#'
#' When `nextElem(rwalk)` is called, the generator executes its
#' "inside" expression until it reaches a call to `yield().` THe
#' generator 'pauses', preserving its execution state, and `nextElem`
#' then returns what was passed to `yield`. The next time
#' `nextElem(rwalk)` is called, the generator resumes executing its
#' inside expression starting after the `yield()`
#'
#' The generator expression is evaluated in a local environment.
#'
#' Generators are _not_ based on forking or parallel OS processes; they
#' run in the same R process where `nextElem` is called.
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
#' @return `gen({...}) returns an [iteror].
#' @param compileLevel Current levels are 0 (no compilation) or
#' -1 (name munging only).
#' @export
gen <- function(expr, ..., split_pipes=FALSE, trace=trace_,
                compileLevel=get("compileLevel", parent.env(environment()))) {
  expr <- arg(expr)
  args_ <- c(cps_translate(expr,
                           endpoints=gen_endpoints,
                           split_pipes=split_pipes),
             orig=forced_quo(expr),
             trace=arg(trace),
             dots(...))
  set_dots(environment(), args_)
  gen <- make_generator(...)
  if (compileLevel != 0) gen <- compile(gen, compileLevel)
  gen
}

#' @export
#' @rdname gen
#' @description
#' When written inside a generator expression, `yield(expr)` causes the
#' generator to return the given value, then pause until the next value is
#' requested.
#' @return `yield(x)` returns the same value x.
#'
yield <- function(expr) {
  stop("yield() called outside a generator")
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

#' @export
#' @rdname gen
#' @description
#'
#' When running in a generator expression, `yieldFrom(it)`, given
#' a list or [iteror] in its argument, will yield successive values from that
#' iteror until it is exhausted, then continue.
#'
#' @param it A list, [iteror] or compatible object.
#' @return yieldFrom returns NULL, invisibly.
#' @examples
#' chain <- function(...) {
#'   iterators <- list(...)
#'   gen(for (it in iterators) yieldFrom(it))
#' }
yieldFrom <- function(it) {
  stop("yieldFrom() called outside a generator")
}

yieldFrom_cps <- function(it) {
  function(cont, ..., yield, trace=trace) {
    list(cont, yield, trace)

    yieldFrom_ <- function(val) {
      stopping <- FALSE
      trace("yieldFrom: next")
      val <- nextElemOr(iter, stopping <- TRUE)
      if (stopping) {
        trace("yieldFrom: stopping")
        cont(invisible(NULL))
      } else {
        yield(val, yieldFrom_)
      }
    }

    iter <- NULL
    iter_ <- function(val) {
      iter <<- iteror(val)
      trace("yieldFrom: got iteror")
      yieldFrom_(NULL)
    }

    it(iter_, ..., yield=yield, trace=trace)
  }
}

make_generator <- function(expr, orig=arg(expr), ..., trace=trace_) { list(expr, ...)
  gen_cps <- function(expr) { force(expr)
    function(cont, ..., stop, return, pause, trace) {
      list(stop, return, pause, trace)
      nonce <- function() NULL
      yielded <- nonce
      err <- nonce
      state <- "yielded"

      getState <- function() state

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
      }

      yield_ <- function(val, cont, ...) {
        trace("generator: yield\n")
        yielded <<- val
        state <<- "yielded"
        pause(cont, ...)
      }

      nextElemOr_ <- function(or, ...) {
        trace("generator: nextElemOr\n")
        val <- switch(state,
               stopped =,
               finished = or,
               running = base::stop("Generator already running (or finished unexpectedly?)"),
               yielded = {
                 state <<- "running"
                 on.exit({
                   if (state == "running") {
                     state <<- "finished"
                     #just a warning  (or not) bc we're probably on our
                     #way out with a more detailed error
                     #warning("Generator finished unexpectedly")
                   }
                 })
                 pump()
                 switch(state,
                        running = {
                          state <<- "finished"
                          base::stop("Generator finished unexpectedly")
                        },
                        stopped = base::stop(err),
                        finished = or,
                        yielded = {
                          if (identical(yielded, nonce)) {
                            base::stop("Generator yielded but no value?")
                          }
                          tmp <- yielded
                          yielded <<- nonce
                          tmp
                        },
                        base::stop("Generator in an unknown state"))
               },
               base::stop("Generator in an unknown state"))
        val
      }

      nextElemOr_ <<- nextElemOr_
      expr(return_, ..., stop=stop_, return=return_, yield=yield_,
           pause=pause, trace=trace)
    }
  }

  nextElemOr_ <- NULL
  pump <- make_pump(gen_cps(expr), trace=trace, catch=FALSE)
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
  #everything whose name you want to remain stable after munging,
  #for instance so the above accessors can find them.
  list(entry=getEntry(x),
       stop_=getStop(x),
       return_=getReturn(x),
       pump=get("pump", envir=environment(x$nextElemOr)),
       runPump=environment(get("pump", environment(x$nextElemOr)))$runPump,
       doWindup=environment(get("pump", environment(x$nextElemOr)))$doWindup,
       nextElemOr=x$nextElemOr,
       getState=get("getState", environment(x$nextElemOr)))
}

#' @exportS3Method
compile.generator <- function(x, level) {
  # returns an environment with munged nodes/storage
  if (abs(level) >= 1) {
    munged <- munge( x )
    munged$orig <- get("orig", environment(x$nextElemOr))
    if (abs(level) >= 3) {
      stop("TODO: Aggressive inlining")
    } else if (abs(level) >= 2) {
      stop("TODO: Inlining")
    }
    # create a new iteror with this munged generator's nextElemOr.
    if (level <= -1) {
      new <- add_class(iteror(munged$nextElemOr), c("generator"))
      if (paranoid) { # enabled in unit tests
        expect_properly_munged(x, new)
      }
      new
    } else if (level >= 1) {
      stop("TODO: code generation")
    }
  } else x
}

#' @export
print.generator <- function(x, ...) {
  cat(format(x, ...), sep="\n")
}

getState <- function(x, ...) {
  UseMethod("getState")
}

getState.generator <- function(x, ...) {
  environment(x$nextElemOr)$getState()
}

#' @export
format.generator <- function(x, ...) {
  envir <- environment(x$nextElemOr)
  code <- get("orig", envir)
  a <- deparse(call("gen", expr(code)), backtick=TRUE)
  b <- format(env(code), ...)
  state <- getState(x)
  c <- paste0(c("<Generator [",
         if(state=="stopped")
           c("stopped: ", capture.output(print(envir$err)))
         else state,
         "]>"), collapse="")
  c(a, b, c)
}
