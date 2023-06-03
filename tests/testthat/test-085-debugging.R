fn <- function() {
  x
}
setCompileLevelFromFn(fn)

test_that("tracing execution", {

  op <- capture.output({
    r <- run({
      x <- 0
      while(x < 10) {
        yield(x <- x + 1)
      }
    }, trace=cat)
  })
  expect_equal(length(grep("x <- x \\+ 1", op)), 10)

  g <- gen({
    x <- 0
    while(x < 10) {
      yield(x <- x + 1)
    }
  }, trace=cat)
  op <- capture.output(invisible(as.list(g)))
  expect_equal(length(grep("\\(pause_val\\)", op)), 10)

  m <- mock_promise()
  op <- capture.output({
    a <- async({
      await(m) + 5
    }, trace=cat)
    m$resolve(10)
    wait_for_it()
  })
  expect_equal(length(grep("\\(pause\\)", op)), 1)

  mch <- mock_channel()
  st <- stream({
    x <- 0
    for (i in mch$ch) {
      yield(x <- x + 1)
    }
  }, trace=cat)
  mch$emit(1)
  mch$emit(2)
  op <- capture.output({
    res <- gather(st)
    wait_for_it()
    mch$emit(3)
    mch$finish()
    wait_for_it()
  })

  expect_equal(length(grep("x <- x \\+ 1", op)), 3)
  # this is 4 when running under testthat???
  #expect_equal(length(grep("\\(pause\\)", op)), 2)
  expect_equal(length(grep("\\(exit: return\\)", op)), 1)

  g <- gen({
    x <- 0
    while(x < 10) {
      yield(x <- x + 1)
    }
  })
  expect_silent({
    nextOr(g)
    debugAsync(g, trace=TRUE)
  })
  op <- capture.output({
    nextOr(g)
    nextOr(g)
    invisible(debugAsync(g, trace=FALSE))
  })
  expect_silent(as.list(g))
  expect_equal(length(grep("x <- x \\+ 1", op)), 2)

})


# Is it possible to test single-stepping/breaking?
