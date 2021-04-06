#' @import nseval

`%is%` <- expect_equal

test_that("generators", {
  x <- make_generator(`{_cps`(yield_cps(arg_cps("one")),
                              yield_cps(arg_cps("two"))))
  as.list(x) %is% list("one", "two")

  x <- make_generator(`{_cps`(yield_cps(arg_cps("one")),
                              yield_cps(arg_cps("two")),
                              arg_cps(print("threeeee"))))
  expect_output(as.list(x) %is% list("one", "two"), "threee")
})

test_that("generator loop", {
  ii <- function(n) make_generator(for_cps(arg_cps(i),
                                           arg_cps(1:n),
                                           yield_cps(arg_cps(i))))
  i <- ii(10)
  for (j in 1:10) {
    nextElem(i) %is% j
  }
  expect_error(nextElem(i), "StopIteration")

  i <- itertools::ihasNext(ii(10))
  for (j in 1:10) {
    itertools::hasNext(i) %is% TRUE
    nextElem(i) %is% j
  }
  itertools::hasNext(i) %is% FALSE
  expect_error(nextElem(i), "StopIteration")
})

