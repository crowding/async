`%is%` <- expect_equal

level <- -1

before_and_after <- function(expr, and_then, ...) {
  a <- nseval::arg(expr)
  list(nseval::do(a), and_then(nseval::do(a), ...))
}

test_that("can compile generator and print it", {
  g <- async:::compile(gen(for (i in 1:10) yield(i)), level=level)
  expect_output(print(g))
})

test_that("name munging generators", {

  # walk the graph before and after compile and compare them.
  g <- gen({
    x <- 0
    for (i in 1:10)
              yield(x <- x + i)
  })
  graph <- walk(g)
  gc <- compile(g, level=level)
  graphc <- walk(gc)

  # the names assigned walking the graph should become local names of graphc
  cnodes <- vapply(names(graphc$nodes),
                  function(nodeName)
                    graphc$nodeProperties[[nodeName]]$"localName",
                  "")
  expect_true(setequal(unname(cnodes), names(graph$nodes)))

  # Walking gc should have only picked up one context
  expect_equal(length(graphc$contexts), 1)
  env <- as.list(graphc$contexts)[[1]]
  con <- names(graphc$contexts)
  # all nodes are closed in that context
  for (nod in as.list(graphc$contextNodes[[con]])) {
    expect_identical(environment(graphc$nodes[[nod]]), env)
  }

  # and state pointers point to functions in the same context.
  for (var in graphc$contextProperties[[con, "external"]]) {
    val <- graphc$contexts[[con]][[var]]
    if (is.function(val) && !is.quotation(val) && !is.null(body(val))) {
      expect_identical(environment(val), env)
    }
  }

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
# this is meant to test that multiple scopes with similar bindings are
# disambiguated

  g <- before_and_after(
    gen({
      x <- 0
      for (i in 1:10) {
        x <- x + i
        for (i in 1:10)
          yield(x <- x + i)
        x <- x - i
      }
    }),
    compile, level=-1)

  expect_equal(as.list(g[[1]]), as.list(g[[2]]))
})
