`%is%` <- expect_equal

before_and_after <- function(expr, and_then, ...) {
  a <- nseval::arg(expr)
  list(nseval::do(a), and_then(nseval::do(a), ...))
}

test_that("can compile generator and print it", {
  g <- gen(for (i in 1:10) yield(i))
  gc <- async:::compile(g, level=-1)
  expect_output(print(gc))
})

test_that("name munging generators", {

  fg <- function(compileLevel=0) gen({
    x <- 0
    for (i in 1:10)
              yield(x <- x + i)
  }, compileLevel=compileLevel)

  g <- fg()
  gc <- fg(compileLevel=-1)

  # walk the graph before and after compile and compare them.
  expect_properly_munged(g, gc)

  # Can we pull a next element?
  nextElemOr(g)
  nextElemOr(gc)

  # finally, test function result
  expect_equal(as.list(g), as.list(gc))
})

test_that("munged generator has compatible parent environment", {
  total <- 0
  g <- gen(for (i in 1:10) total <<- yield(total + i))
  gc <- compile(g, level=-1)
  expect_equal(as.list(gc), as.list(cumsum(1:10)))
  expect_equal(total, sum(1:10))
})

test_that("nested loops", {
  # this is meant to test that multiple scopes with similar
  # bindings are
  # disambiguated and 'for' plays by the same rules

  fg <- function() gen({
      x <- 0
      for (i in 1:10) {
        x <- x + i
        for (i in 1:10)
          yield(x <- x + i)
        x <- x - i
      }
  })
  g <- fg()
  gc <- compile(fg(), level=-1)
  expect_equal(as.list(g), as.list(gc))

})

test_that("can compile and print async", {
  p <- mock_promise()
  a <- async(await(p) + 1, compileLevel=-1)
  expect_output(print(a))
})

test_that("munged async with a try/finally", {

  pr <- mock_promise()
  # async(await(p) + 1, ...)
  fa <- function(...) async({
    tryCatch({
      if (val <- await(pr)) {
        return(5)
        val
      } else 4
    }, finally={
      print("cleaned up")
    })
  }, ...)

  a <- fa(compileLevel=0)
  ac <- fa(compileLevel=-1)
  expect_properly_munged(a, ac)
  expect_output(
    expect_resolves_with(
      a, 5, expect_resolves_with(
        ac, 5, pr$resolve(100))),
    "cleaned up")

})
