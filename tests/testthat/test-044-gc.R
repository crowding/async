fn <- function() {
  x
}
setCompileLevelFromFn(fn)

# Two things needed to be done:
# One issue appears to be "orig" quoted holding on to the original
# environment, esp. in a generator-function, async-function, etc.
# Another issue was an unforced promise for .contextName in async:::R.

test_that("finished generators should be smaller when serialized", {

  g <- gen(x <- yield(runif(100000)))
  l1 <- length(serialize(g, NULL))
  nextOr(g, NULL)
  l2 <- length(serialize(g, NULL))
  nextOr(g, NULL)
  l3 <- length(serialize(g, NULL))

  expect_gt(l2-l1, 500000)
  expect_gt(l2-l3, 500000)
})

test_that("finished generators should be smaller when serialized", {

  n <- 1
  g <- gen({
    for(i in seq_len(n)) {
      x <- runif(100000)
      yield(mean(x))
    }
  })
  l1 <- length(serialize(g, NULL))
  nextOr(g, NULL)
  l2 <- length(serialize(g, NULL))
  nextOr(g, NULL)
  l3 <- length(serialize(g, NULL))

  expect_gt(l2-l1, 500000)
  expect_gt(l2-l3, 500000)

})

test_that("finished generator functions should be smaller when serialized", {

  g <- gen(function(n, len) {
    for (i in seq_len(n)) {
      x <- runif(len)
      yield(mean(x))
    }
  })

  it <- g(2, 123456)
  r1 <- serialize(it, NULL)
  nextOr(it)
  r2 <- serialize(it, NULL)
  expect_gt(length(r2) - length(r1), 600000)
  nextOr(it)
  r3 <- serialize(it, NULL)
  expect_null(nextOr(it, NULL))
  r4 <- serialize(it, NULL)
  expect_gt(length(r3) - length(r4), 600000)

})

test_that("finished generator functions should release memory", {

  g <- gen(function(n, len) {
    for (i in seq_len(n)) {
      x <- runif(len)
      yield(mean(x))
    }
  })

  m0 <- gc(full=TRUE)
  it <- g(2, 100000)
  m1 <- gc(full=TRUE)
  nextOr(it)
  m2 <- gc(full=TRUE)
  expect_gt((m2-m1)["Vcells" ,"used"], 80000)
  nextOr(it)
  m3 <- gc(full=TRUE)
  expect_null(nextOr(it, NULL))
  m4 <- gc(full=TRUE)
  expect_gt((m3-m4)["Vcells" ,"used"], 80000)
  rm(g)
  m5 <- gc(full=TRUE)

})

test_that("forgotten generators should garbage collect?!", {

  f <- function() {

    g <- gen(function(n, len) {
      for (i in seq_len(n)) {
        x <- runif(len)
        yield(mean(x))
      }
    })

    it <- g(2, 100000)
    m1 <- gc(full=TRUE)
    nextOr(it)
    m2 <- gc(full=TRUE)
    it <- NULL
    m3 <- gc(full=TRUE)
    expect_gt((m2-m3)["Vcells", "used"], 80000)

  }

  f()
})

serialized_has <- function(x, pattern)
( x
  |> serialize(NULL, ascii=TRUE)
  |> rawToChar() |> strsplit("\n")
  |> grepl(x=_, pattern)
  |> any()
)

test_that("finished async-functions should forget their environments", {

  nonmagic <- "123456788"
  m <- mock_promise()
  af <- async(function(x) {
    value <- as.integer(nonmagic) + 1L
    await(x)
  })
  magic <- paste0("1234", "56789") # avoiding "srcref" shenanigans...
  as <- af(m)
  wait_for_it()
  expect_true(serialized_has(as, magic))
  m$resolve(5)
  wait_for_it()
  expect_false(serialized_has(as, magic))

})

test_that("finished stream-functions should forget their environments", {

  nonmagic <- "123456788"
  m <- mock_promise()
  sf <- stream(function(x) {
    value <- as.integer(nonmagic) + 1L
    yield(1)
    await(x)
  })

  magic <- paste0("1234", "56789") # avoiding "srcref" shenanigans...
  st <- sf(m)
  pr <- nextOr(st)
  wait_for_it()
  expect_true(serialized_has(st, magic))
  m$resolve(5)
  wait_for_it()
  nextOr(st, NULL)

  expect_false(serialized_has(as, magic))

})
