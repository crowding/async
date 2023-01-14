
#' Execute a generator expression immediately, collecting values.
#'
#' `run(expr)` with an expression directly writen, will transform that
#' expression to run asynchronously, but then run it without pausing;
#' any values yielded are returned in a list.
#'
#' If the expression contains any calls to `yield()`, `run()` will
#' collect all the values passed to yield() and return a list. If the
#' expression contains a `yield()` but it is never called, `run()`
#' returns an empty list. If the expression does not contain a `yield`
#' at all, `run` returns the expressions' final value.
#'
#' `run(expr)` is similar to `as.list(gen(expr))`, except `run(expr)`
#' evaluates its expression directly in the calling environment, while
#' `gen` creates a new enclosed environment to run in.
#'
#' `run` is useful if you want to take advantage of a generator
#' expression's extensions, such as using `for` loops over iterators,
#' or using [goto()] in `switch` statements, in otherwise synchronous
#' code. If you want to collect a variable-length sequence of values
#' but don't need those features, using [collect] directly will be
#' more efficient.
#'
#' @examples
#' run(for (i in iseq(2, 5, Inf)) if (i %% 37 == 0 break) else yield(i)))
#'
#' @param expr A generator expression, same as you would write in
#'   [gen]().
#' @param type A value which will determine the output vector type (as
#'   in [vapply].)
#' @param ... Undocumented.
#' @param split_pipes See [async]; defaults to FALSE.
#' @param return If `expr` contains any `yield` calls, a vector of the
#'   same mode as `type`. Otherwise the return value of the
#'   expression.
#' @param debugR Will open a browser at the first and subsequent R
#'   evaluations allowing single-stepping through user code.
#' @param debugInternal Will set a breakpoint at the implementation
#'   level, allowing single-stepping through `async` package code.
#' @export
run <- function(expr, type=list(), ..., split_pipes=FALSE, trace=trace_,
                debugR=FALSE, debugInternal=FALSE) {
  .contextName <- "run"
  expr <- arg(expr)
  envir <- env(expr)
  set_arg(expr_, cps_translate(expr,
                           endpoints=gen_endpoints,
                           split_pipes=split_pipes))
  state <- "running"
  result <- NULL
  collecting <- FALSE
  registerYield_ <- function() collecting <<- TRUE
  return_ <- function(val) {result <<- val; state <<- "finished"}
  stop_ <- function(err) {result <<- err; state <<- "stopped"}
  y <- function(val, name=NULL) stop("Yield was not registered")
  yield <- function(cont, val) {val <- y(val); cont(val)}
  pump <- make_pump(expr_, ..., targetEnv=envir, registerYield=registerYield_,
                    rtn=return_, stp=stop_, yield=yield, catch=FALSE)
  environment(pump)$setDebug(R=debugR, internal=debugInternal)

  go <- function() repeat switch(state,
                    running={ state <<- "active"; pump() },
                    stopped=stop(result),
                    finished=return(result),
                    active=,
                    stop(paste0("Run finished unexpectedly: ", state)))

  if (collecting) {
    collect(type=type, function(yield) {
      y <<- yield
      go()
    })
  } else {
    go()
  }
}