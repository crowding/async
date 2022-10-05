`%is%` <- expect_equal

orig_and_compiled <- function(expr, and_then, ...) {
  a <- arg(expr)
  list(do(a), and_then(do(a), ...))
}

test_that("can print compiled generator", {
  g <- async:::compile(gen(for (i in 1:10) yield(i)))
  format(g)
})

test_that("all nodes of compiled generator have same environment", {
  g <- orig_and_compiled(
    gen({
      x <- 0
      for (i in 1:10)
        yield(x <- x + i)
    }),
    async:::compile, level=1)

  graph <- walk(g[[1]])
  graphc <- walk(g[[2]])

  # now check the environments on the second are all identical
  e <- NULL

  for (i in names(graphc$nodes)) {
    if (is.null(e))
      e <- environment(graphc$nodes[[i]])
    else
      expect_identical(e, graphc$nodes[[i]])
  }

  # only finally test function.
  expect_equal(as.list(g[[1]]), as.list(g[[2]]))
})

test_that("munged generator has compatible parent environment", {
  total <- 0
  g <- gen (for (i in 1:10) total <<- total + yield(i))
  expect_equal(as.list(gen), as.list(1:10))
  expect_equal(total, sum(1:10))
})

test_that("nested loops", {
# this is meant to test that the 

  g <- orig_and_compiled(
    gen({
      x <- 0
      for (i in 1:10) {
        x <- x + i
        for (i in 1:10)
          yield(x <- x + i)
        x <- x - i
      }
    }),
    compile, level=1)

  expect_equal(as.list(g[[1]]), as.list(g[[2]]))
})
