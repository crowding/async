#' `%~%` defines a sort of function that runs in its parent scope,
#' without creating a local scope.
#'
#' Supply a variable name on the left; that one will be bound in the
#' present environment.
#'
#' One use for this is to supply as an error handler in `tryCatch`.
#' Evaluating in the parent scope means that you can use control flow
#' constructs like "break" or "next".
#'
#' The price you pay for this is that your argument variable is bound
#' in the present environment and might overwrite something already
#' there.
#'
#' This allows you to do things like consuming an old-style
#' [iterators::iterator] using `nextElem` in a loop.
#'
#' @examples
#'
#'  # go through a list of iterators and print them each on one line
#'  iters <- list(iseq(-10, 10, 5), iseq(12, -12, -6))
#'  for (it in iters) {
#'    it <- iteror(it)
#'    tryCatch({
#'      repeat cat(nextElem(it), "")
#'    }, error=x %~% if (conditionMessage(x) == "StopIteration") {
#'      cat("\n");
#'      next                  # we can use "next" in the handler!
#'    } else stop(x))
#'  }
`%~%` <- function(var, expr) {
  function(val) {
    var <- arg(var)
    expr <- arg(expr)
    set_(var, val)
    value(expr)
  }
}

test_that("%~%", {

  # loop over and collect several iterators:
  iters <- list(iseq(-10, 10, 5), iseq(12, -12, -6))
  m <- collect(type=0, \(emit) {
    for (it in iters) {
      it <- iteror(it)
      tryCatch({
        repeat emit(nextElem(it))
      }, error = x %~% if (conditionMessage(x) == "StopIteration")
        next
      else stop(x))
    }
  })
  m %is% c(-10,  -5,   0,   5,  10,  12,   6,   0,  -6, -12)

})

# now how would you implement detecting %~% in CPS?
