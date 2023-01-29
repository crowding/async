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
#' On the "outside," that is, the object returned by `gen()`, a
#' generator behaves like an iterator over an indefinite
#' collection. So we can collect the first 100 values from the above
#' generator and compute their mean:
#'
#' ```r
#' rwalk |> itertools2::take(100) |> as.numeric() |> mean()
#' ```
#' When `nextElem(rwalk)` is called, the generator executes its
#' "inside" expression until it reaches a call to `yield().` THe
#' generator 'pauses', preserving its execution state, and `nextElem`
#' then returns what was passed to `yield`. The next time
#' `nextElem(rwalk)` is called, the generator resumes executing its
#' inside expression starting after the `yield()`.
#'
#' The generator expression is evaluated in a local environment.
#'
#' Generators are _not_ based on forking or parallel OS processes; they
#' run in the same R process where `nextElem` is called.
#'
#' A generator expression can use any R functions, but a call to
#' `yield` may only appear in the arguments of a "pausable" function.
#' The `async` package has several built-in pausable functions corresponding
#' to base R's control flow functions, such as `if`, `while`, `tryCatch`,
#' `<-`, `{}`, `||` and so on (see [pausables] for more details.)  A call
#' to `yield` may only appear in an argument of one of these pausable
#' functions. So this random walk generator:
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
  envir <- arg_env(expr)
  expr <- arg(expr)
  .contextName <- "wrapper"
  args_ <- c(cps_translate(expr,
                           endpoints=gen_endpoints,
                           split_pipes=split_pipes),
             orig=forced_quo(expr),
             trace=arg(trace),
             dots(...))
  set_dots(environment(), args_)
  gen <- make_generator(..., targetEnv=new.env(parent=envir))
  if (compileLevel != 0) gen <- compile(gen, compileLevel)
  gen
}

#' @description
#' When written inside a generator expression, `yield(expr)` causes the
#' generator to return the given value, then pause until the next value is
#' requested.
#' @return `yield(x)` returns the same value x.
#' @rdname gen
#' @export
yield <- function(expr) {
  stop("yield() called outside a generator")
}

yield_cps <- function(.contextName, expr) {
  list(.contextName, expr)
  function(cont, ..., yield, registerYield, trace) {
    if (missing_(arg(yield))) stop("yield used but this is not a generator")
    if (!missing(registerYield)) registerYield()
    list(cont, yield, trace)
    `yield_` %<-% function(val) {
      force(val)
      trace("yield\n")
      yield(cont, val)
    }
    expr(yield_, ..., yield=yield, registerYield=registerYield, trace=trace)
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

yieldFrom_cps <- function(.contextName, it) {
  list(.contextName, it)
  function(cont, ..., yield, trace=trace, registerYield) {
    list(cont, yield, trace, maybe(registerYield))
    if (!is_missing(registerYield)) registerYield()

    yieldFrom_ %<-% function(val) {
      stopping <- FALSE
      trace("yieldFrom: next\n")
      val <- nextElemOr(iter, stopping <- TRUE)
      if (stopping) {
        trace("yieldFrom: stopping\n")
        cont(invisible(NULL))
      } else {
        yield(yieldFrom_, val) #FIXME: under a "run" this winds up the stack
      }
    }

    iter <- NULL
    iter_ %<-% function(val) {
      iter <<- iteror(val)
      trace("yieldFrom: got iteror")
      yieldFrom_(NULL)
    }

    it(iter_, ..., yield=yield, trace=trace, registerYield=registerYield)
  }
}

make_generator <- function(expr, orig=arg(expr), ..., trace=trace_) {
  list(expr, ..., orig, trace)
  .contextName <- "gen"
  # gen_cps <- function(.contextName, expr) {
  #   list(.contextName, expr)
  #     list(stp, rtn, pause, pause_val, trace)
  nonce <- function() NULL
  yielded <- nonce
  err <- nonce
  state <- "yielded"

  getState %<-% function() state

  return_ %<-% function(val) {
    force(val)
    trace("generator: return\n")
    state <<- "finished"
  }

  stop_ %<-% function(val) {
    trace("generator: stop\n")
    err <<- val
    state <<- "stopped"
    stop(val)
    NULL #so the above does not look like a tailcall
  }

  yield_ %<-% function(cont, val) {
    trace("generator: yield\n")
    state <<- "yielded"
    yielded <<- val
    pause_val(cont, val)
  }

  nextElemOr_ %<g-% function(or, ...) {
    trace("generator: nextElemOr\n")
    val <- switch(state,
                  "stopped" =,
                  "finished" = or,
                  "running" = stop("Generator already running (or finished unexpectedly?)"),
                  "yielded" = {
                    state <<- "running"
                    on.exit({
                      if (state == "running") {
                        state <<- "finished"
                        #we're probably on our way out with a more
                        #detailed error
                      }
                    })
                    # silly compiler, that's not a "utility" call
                    (function() pump())()
                    switch(state,
                           running = {
                             state <<- "finished"
                             stop("Generator finished unexpectedly")
                           },
                           stopped = stop(err),
                           finished = or,
                           yielded = {
                             if (identical(yielded, nonce)) {
                               stop("Generator yielded but no value?")
                             }
                             tmp <- yielded
                             yielded <<- nonce
                             tmp
                           },
                           stop("Generator in an unknown state"))
                  },
                  stop("Generator in an unknown state"))
    val
  }

  pump <- make_pump(expr, ..., trace=trace, catch=FALSE,
                    stp=stop_, yield=yield_, rtn=return_)
  pause_val <- get("pause_val_", envir=environment(pump))

  g <- add_class(iteror(nextElemOr_), "generator", "coroutine")
  g
}

#' @exportS3Method
getCurrent.generator <- function(x)
  environment(getPump(x))$cont
#' @exportS3Method
getOrig.generator <- function(x) {
  expr(get("orig", envir=environment(x$nextElemOr)))
}

#' @exportS3Method
getStartSet.generator <- function(x) {
  c(NextMethod(x), list(
    nextElemOr_=  x$nextElemOr,
    #doWindup = environment(getPump(x))$doWindup),
    getState = environment(x$nextElemOr)$getState))
}

#' @exportS3Method
reconstitute.generator <- function(orig, munged) {
  environment(munged$nextElemOr_)$orig <- environment(orig$nextElemOr)$orig
  new <- add_class(iteror(munged$nextElemOr_), "generator", "coroutine")
  new
}

#' @exportS3Method
getState.generator <- function(x, ...) {
  environment(x$nextElemOr)$getState()
}

#' @exportS3Method
getPump.generator <- function(x) get("pump", environment(x$nextElemOr))

#' @exportS3Method
#' @rdname format
getNode.generator <- function(x, ...) {
  environment(getPump(x))$getCont()
}

#' Query / display coroutine properties and state.
#' `format.generator` displays the original code given
#'   to construct the generator, its bound environment, whether it is running
#'   or finished, and a label indicating it last known state.
#'
#' `getState.generator` retreives the current state of a
#' generator. This might be "yielded", "running" (if nextElem is
#' _currently_ being called), "stopped" (for generators that have
#' stopped with an error) or "finished" (for generators that have
#' finished normally.)
#'
#' `getState.async` might return "running", "awaiting", "resolved" or
#' "rejected".
#'
#' `getState.stream` might return "running", "awaiting", "yielding",
#' "yielded", "resolved" or "rejected".
#'
#' `getNode` returns a string identifier indicating where a
#' coroutine's execution was last paused. The string is like an
#' adsress pointing to a spot in the orginal expression's parse tree;
#' a string like `.{1.<-2.await__then` can be read like
#' "in the first argument of `{`, in the second argument of `<-`, in a
#' call to await(), at internal node `then`."
#'
#' `getOrig` returns the original expression given to the generator
#' constructor.
#' @exportS3Method
#' @rdname format
format.generator <- function(x, ...) {
  envir <- environment(x$nextElemOr)
  code <- getOrig(x)
  a <- deparse(call("gen", code), backtick=TRUE)
  b <- format(envir, ...)
  state <- getState(x)
  cont <- getNode(x)
  c <- paste0(c("<generator [",
                state, " at `", cont, "`",
                if (state=="stopped")
                  c(": ", capture.output(print(envir$err))),
                "]>"), collapse="")
  c(a, b, c)
}
