test_that("a generator", {
  x <- gen(for (i in 1:10) yield(i))
  as.numeric(as.list(x))
  x %is% 1:10
})

test_that("for loop over an iterator", {
  x <- gen(for (i in icount()) {yield(i)})
  as.numeric(as.list(itertools::ilimit(x, 10))) %is% 1:10
})
