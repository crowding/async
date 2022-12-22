test_that("Set compiler level", {
  expect_equal(  asyncOpts()$compileLevel, 0)
  g <- gen(repeat yield(TRUE))
  expect_false(exists("entry", environment(g$nextElemOr)))
  asyncOpts(compileLevel=-1)
  expect_equal(async:::compileLevel, -1)
  gc <- gen(repeat yield(TRUE))
  expect_true(exists("entry", environment(gc$nextElemOr)))
})
