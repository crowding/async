`%is%` <- expect_equal

test_that("a generator", {
  x <- gen(for (i in 1:10) yield(i))
  as.numeric(as.list(x)) %is% 1:10
})

test_that("for loop over an iterator", {
  x <- gen(for (i in icount()) {yield(i)})
  as.numeric(as.list(itertools::ilimit(x, 10))) %is% 1:10
})

test_that("nested for loops", {
  x <- gen({
    for (i in iterators::iter(c(3,2,1,2), recycle=TRUE)) {
      for (j in 1:i)
        yield(1)
      yield(0)
    }
  })
  as.numeric(as.list(itertools::ilimit(x, 24))) %is%
    c(1,1,1,0,1,1,0,1,0,1,1,0,1,1,1,0,1,1,0,1,0,1,1,0)
})
