`%is%` <- expect_equal

level <- -1

before_and_after <- function(expr, and_then, ...) {
  a <- nseval::arg(expr)
  list(nseval::do(a), and_then(nseval::do(a), ...))
}

test_that("can compile generator and print it", {
  g <- gen(for (i in 1:10) yield(i))
  gc <- async:::compile(g, level=level)
  expect_output(print(gc))
})

expect_graph_names_are_graphc_locals <- function(graph, graphc) {
  # the names assigned walking the graph should become local names of
  # graphc.
  cnodes <- vapply(names(graphc$nodes),
                   function(nodeName)
                     graphc$nodeProperties[[nodeName]]$"localName",
                   "")
  if (!setequal(unname(cnodes), names(graph$nodes))) browser()
  expect_true(setequal(unname(cnodes), names(graph$nodes)))
}

expect_only_one_context <- function(graphc) {
  # Walking gc should have only picked up one context
  expect_equal(length(graphc$contexts), 1)
  env <- as.list(graphc$contexts)[[1]]
  con <- names(graphc$contexts)
  # all nodes are closed in that context
  for (nod in as.list(graphc$contextNodes[[con]])) {
    expect_identical(environment(graphc$nodes[[nod]]), env)
  }
}

expect_state_pointers_closed <- function(graphc) {
  # and function pointers point to functions in the same context.
  con <- names(graphc$contexts)
  env <- as.list(graphc$contexts)[[1]]
  for (var in unique(c(
    graphc$contextProperties[[con, "read"]],
    graphc$contextProperties[[con, "store"]]))) {
    val <- graphc$contexts[[con]][[var]]
    if (is.function(val) && !is.quotation(val) && !is.null(body(val))) {
      if (!identical(environment(val), env)) browser()
      expect_identical(environment(val), env)
    }
  }
}

test_that("name munging generators", {

  fg <- function() gen({
    x <- 0
    for (i in 1:10)
              yield(x <- x + i)
  })
  # walk the graph before and after compile and compare them.
  # Note that a munged generator's R_ nodes
  # still have the same target environment. So invoke gen twice.
  g <- fg()
  gc <- compile(fg(), level=-1)
  graph <- walk(g)
  # should be able to walk the same graph again...
  graphc <- walk(gc)

  expect_graph_names_are_graphc_locals(graph, graphc)
  expect_only_one_context(graphc)
  expect_state_pointers_closed(graphc)

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

test_that("munged async", {
  p <- mock_promise()
  fa <- function(...) async(await(p) + 1, ...)
  munged <- munge(a)

  a <- fa()
  ac <- fa(compileLevel=-1)
  graph <- walk(a)
  # should be able to walk the same graph again...
  graphc <- walk(ac)

  expect_graph_names_are_graphc_locals(graph, graphc)
  expect_only_one_context(graphc)
  expect_state_pointers_closed(graphc)

})
