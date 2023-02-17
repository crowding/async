test_that("Compiler level -1 does munging", {
  expect_equal(getOption("async.compileLevel"), 0)
  g <- gen(repeat yield(TRUE))
  expect_false(exists("entry", environment(g$nextOr)))

  p <- mock_promise()
  value <- NULL
  a <- async(await(p) + 1)
  expect_false(exists("entry", a$state))
  then(a, function(x) value <<- x)
  p$resolve(100)
  wait_for_it()
  expect_equal(value, 101)

  options(async.compileLevel=-1, async.verbose=FALSE, async.paranoid=TRUE)
  expect_equal(getOption("async.compileLevel"), -1)

  gc <- gen(repeat yield(TRUE))
  expect_true(exists("entry", environment(gc$nextOr)))

  p <- mock_promise()
  value <- NULL
  ac <- async(await(p) + 1)
  then(ac, function(x) value <<- x)
  p$resolve(100)
  wait_for_it()
  expect_equal(value, 101)

})
