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
#'     rwalk |> itertools2::take(100) |> as.numeric() |> mean()
#'
#' When `nextOr(rwalk, ...)` is called, the generator executes its
#' "inside" expression, in a local environment, until it reaches a
#' call to `yield().` THe generator 'pauses', preserving its execution
#' state, and `nextElem` then returns what was passed to `yield`. The
#' next time `nextElem(rwalk)` is called, the generator resumes
#' executing its inside expression starting after the `yield()`.
#'
#' If you call `gen` with a function expression, as in:
#'
#'     gseq <- gen(function(x) for (i in 1:x) yield(i))
#'
#' then instead of returning a single generator it will return a
#' _generator function_ (i.e. a function that constructs and returns a
#' generator.) The above is morally equivalent to:
#'
#'     gseq <- function(x) {force(x); gen(for (i in 1:x) yield(i))}
#'
#' so the generator function syntax just saves you writing the [force]
#' call.
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
#' @return `gen({...}) returns an [iteror].
#' @param compileLevel Current levels are 0 (no compilation) or
#' -1 (name munging only).
#' @export
gen <- function(expr, ..., split_pipes=FALSE,
                compileLevel=getOption("async.compileLevel")) {
  expr_ <- arg(expr); expr <- NULL
  if (identical(expr(expr_)[[1]], quote(`function`))) {
    defn <- coroutine_function(expr_,
                               quote(async::gen),
                               ...,
                               split_pipes = split_pipes,
                               compileLevel = compileLevel)
    return(value(defn))
  }
  .contextName <- "wrapper"
  envir <- env(expr_)
  args_ <- c(cps_translate(expr_,
                           endpoints=gen_endpoints,
                           split_pipes=split_pipes),
             orig=forced_quo(expr_),
             dots(...))
  set_dots(environment(), args_)
  gen <- make_generator(..., callingEnv=envir)
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
  function(cont, ..., yield, registerYield) {
    if (missing_(arg(yield))) stop("yield used but this is not a generator")
    if (!missing(registerYield)) registerYield()
    list(cont, yield)
    node(yield_ <- function(val) {
      force(val)
      yield(cont, val)
    })
    expr(yield_, ..., yield=yield, registerYield=registerYield)
  }
}

#' @export
#' @rdname gen
#' @description
#'
#' When running in a generator expression, `yieldFrom(it))`, given
#' a list or [iteror] in its argument, will yield successive values from that
#' iteror until it is exhausted, then continue.
#'
#' @param it A list, [iteror] or compatible object.
#' @param err An error handler
#' @return yieldFrom returns NULL, invisibly.
#' @examples
#' chain <- function(...) {
#'   iterators <- list(...)
#'   gen(for (it in iterators) yieldFrom(it))
#' }
yieldFrom <- function(it, err) {
  stop("yieldFrom() called outside a generator")
}

yieldFrom_cps <- function(.contextName, it) {
  list(.contextName, it)
  function(cont, ..., stp, yield, registerYield, awaitNext) {
    list(cont, yield, maybe(registerYield), maybe(awaitNext))
    if (!is_missing(registerYield)) registerYield()

    result <- "xxx"
    value <- NULL
    node(received <- function() {
      switch(result,
             success={ val <- value; value <<- NULL; yield(streamFrom_, val) },
             error={ stp(value) },
             close=cont(invisible(NULL)),
             stp(paste("Unexpected result from awaitNext", result))) # nocov
    })

    node(streamFrom_ <- function(val) {
      val <- NULL
      result <<- "xxx"
      awaitNext(received,
                iter,
                \(val) {result <<- "success"; value <<- val},
                \(val) {result <<- "error"; value <<- val},
                \() {result <<- "close"; value <<- NULL})
    })

    node(yieldFrom_ <- function(val) {
      stopping <- FALSE
      val <- nextOr(iter, stopping <- TRUE)
      if (stopping) {
        cont(invisible(NULL))
      } else {
        yield(yieldFrom_, val) #FIXME: under a "run" this winds up the stack
      }
    })

    iter <- NULL
    if(is_missing(awaitNext)) {
      node(iter_ <- function(val) {
        iter <<- iteror(val)
        yieldFrom_(NULL)
      })
    } else {
      node(iter_ <- function(val) {
        iter <<- iteror(val)
        if (is.channel(iter)) {
          streamFrom_(NULL)
        } else {
          yieldFrom_(NULL)
        }
      })
    }

    it(iter_, ..., yield=yield, stp=stp, registerYield=registerYield, awaitNext=awaitNext)
  }
}

make_generator <- function(expr, orig=arg(expr), ...,
                           local=TRUE, callingEnv) {
  list(expr, ..., orig)
  .contextName <- "gen"
  targetEnv <- if(local) new.env(parent=callingEnv) else callingEnv
  nonce <- function() NULL
  yielded <- nonce
  err <- nonce
  state <- "yielded"

  node(getState <- function() state)

  node(return_ <- function(val) {
    force(val)
    state <<- "finished"
  })

  node(stop_ <- function(val) {
    err <<- val
    state <<- "stopped"
    stop(val)
    NULL # nocov (so the above does not look like a tailcall)
  })

  node(yield_ <- function(cont, val) {
    state <<- "yielded"
    yielded <<- val
    pause_val(cont, val)
  })

  globalNode(nextOr_ <- function(or, ...) {
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
  })

  pump <- make_pump(expr, ..., catch=FALSE,
                    stp=stop_, yield=yield_, rtn=return_,
                    targetEnv=targetEnv)
  pause_val <- get("pause_val_", envir=environment(pump))

  g <- add_class(iteror(nextOr_), "generator", "coroutine")
  g
}

#' @exportS3Method
getCurrent.generator <- function(x)
  environment(getPump(x))$cont

#' @exportS3Method
getOrig.generator <- function(x, ...) {
  expr(get("orig", envir=environment(x$nextOr)))
}

#' @exportS3Method
getStartSet.generator <- function(x) {
  c(NextMethod(x), list(
    nextOr_=  x$nextOr,
    #doWindup = environment(getPump(x))$doWindup),
    getState = environment(x$nextOr)$getState))
}

#' @exportS3Method
reconstitute.generator <- function(orig, munged) {
  environment(munged$nextOr_)$orig <- environment(orig$nextOr)$orig
  new <- add_class(iteror(munged$nextOr_), "generator", "coroutine")
  new
}

#' @rdname format
#' @return For a [gen]erator `g`, `getState(g)` might return "yielded",
#' "running" (if nextElem is _currently_ being called), "stopped" (for
#' generators that have stopped with an error) or "finished" (for
#' generators that have finished normally.)
#' @exportS3Method
getState.generator <- function(x, ...) {
  environment(x$nextOr)$getState()
}

#' @exportS3Method
getPump.generator <- function(x) {
  get("pump", environment(x$nextOr))
}

#' @exportS3Method
getNode.generator <- function(x, ...) {
  environment(getPump(x))$getCont()
}
