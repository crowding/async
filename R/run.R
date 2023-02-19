#' Execute a generator expression immediately, collecting yielded values.
#'
#' `run(expr)` with an expression directly writen, will parse that
#' expression as a coroutine, but then run it without pausing.
#'
#' If the expression contains any calls to `yield()`, `run()` will
#' collect all the values passed to yield() and return a list. If the
#' expression contains a `yield()` but it is never called, `run()`
#' returns an empty list. If the expression does not contain a `yield`
#' at all, `run` returns the expression's final return value.
#'
#' `run(expr)` is similar to `as.list(gen(expr))`, except `run(expr)`
#' evaluates its expression directly in the calling environment, while
#' `gen` creates a new enclosed environment to run in.
#'
#' `run` is useful if you want to take advantage of coroutine language
#' extensions, such as using `for` loops over iterators, or using
#' [goto()] in `switch` statements, in otherwise synchronous code. If
#' you want to collect a variable-length sequence of values but don't
#' need those features, using [collect] directly will have better
#' performance.
#'
#' @examples
#'
#' run(type=0, {
#'   for (i in iseq(2, Inf, by=5)) {
#'     if (i %% 37 == 0) break
#'     else yield(i)
#'   }
#' })
#'
#' @param expr A generator expression, same as you would write in
#'   [gen].
#' @param type A value whose mode will determine the output vector mode (as
#'   in [vapply].)
#' @param ... Undocumented.
#' @param split_pipes See [async]; defaults to FALSE.
#' @return If `expr` contains any `yield` calls, a vector of the
#'   same mode as `type`; otherwise the return value of `expr`.
#' @param debugR Will open a browser at the first and subsequent R
#'   evaluations allowing single-stepping through user code.
#' @param debugInternal Will set a breakpoint at the implementation
#'   level, allowing single-stepping through `async` package code.
#' @param trace a tracing function.
#' @export
run <- function(expr, type=list(), ..., split_pipes=FALSE, trace=trace_,
                debugR=FALSE, debugInternal=FALSE) {
  expr_ <- arg(expr);
  expr <- NULL
  if (identical(expr(expr_)[[1]], quote(`function`))) {
    defn <- coroutine_function(expr_,
                               quote(async::run),
                               ...,
                               type=type,
                               split_pipes=split_pipes,
                               debugR=debugR,
                               debugInternal=debugInternal)
    return(value(defn))
  }
  .contextName <- "run"
  set_arg_(quo(expr, environment()),
           cps_translate(expr_,
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
  pump <- make_pump(expr, ..., targetEnv=env(expr_), registerYield=registerYield_,
                    rtn=return_, stp=stop_, yield=yield, catch=FALSE)
  environment(pump)$setDebug(R=debugR, internal=debugInternal)

  go <- function() repeat switch(state,
                    running={ state <<- "active"; pump() },
                    stopped=stop(result),
                    finished=return(result),
                    active={}, #error bubbles out
                    stop(paste0("Run finished unexpectedly: ", state))) #nocov

  if (collecting) {
    collect(type=type, function(yield) {
      y <<- yield
      go()
    })
  } else {
    go()
  }
}
