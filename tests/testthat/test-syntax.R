#' @import nseval
`%is%` <- expect_equal

# so, a problem here is that when I devtools-load a package, the
# private functions are loaded into the search path, which means that
# cps_translate finds them. But when I'm running in R CMD CHECK the
# private namespace is not exposed.
#
# But cps_translate, by design, builds an expression with names
# qualified depending on what is visible from the calling environment.
# This makes tests in R CMD check different from testing in devools.
# workaround is to assign them all here:
break_cps <- async:::break_cps
next_cps <- async:::next_cps
cps_translate <- async:::cps_translate
if_cps <- async:::if_cps
repeat_cps <- async:::repeat_cps
for_cps <- async:::for_cps
yield_cps <- async:::yield_cps
`{_cps`  <-  async:::`{_cps`
`<-_cps`  <-  async:::`<-_cps`

ancestors <- function(env) c(
  list(env),
  if({p <- parent.env(env); !identical(p, emptyenv())})
    ancestors(p) else list())

test_that("basic translations", {
  # For the sake of this test, then,
  # make sure these functions are visible:
  expect_warning(cps_translate(quo(x), local=FALSE) %is% quo(async:::R(x)), "keywords")
  cps_translate(quo(break), local=FALSE) %is% quo(break_cps())
  expect_warning(cps_translate(quo(`break`), local=FALSE) %is% quo(async:::R(`break`)), "keywords")
  bonk_cps <- function()function() NULL
  cps_translate(quo(bonk()), endpoints=c("bonk"), local=FALSE) %is% quo(bonk_cps())
  cps_translate(quo(next), local=FALSE) %is% quo(next_cps())
  expect_error(cps_translate(quo(break())), "break")
  expect_warning(cps_translate(quo(2+2), local=FALSE) %is% quo(async:::R(2+2)), "keywords")
  expect_error(cps_translate(quo(list(`break`(4)))), "pausable")
  cps_translate(endpoints="yield",
                quo(if(TRUE) yield(2+2) else yield(4)), local=FALSE) %is%
    quo(if_cps(async:::R(TRUE), yield_cps(async:::R(2 + 2)), yield_cps(async:::R(4))))

  expect_error(cps_translate(quo(flibbert(yield(5))), endpoints="yield"), "flibbert")
  expect_error(cps_translate(quo(rbind(yield(5))), endpoints="yield"), "rbind")
})

test_that("Namespace qualification", {
  cps_translate(quo(repeat async::yield(4)), gen_endpoints) %is%
    quo((function() repeat_cps(async:::yield_cps(async:::R(4))))())

  cps_translate(quo(base::`repeat`(async::yield(4))), gen_endpoints) %is%
    quo((function() async:::repeat_cps(async:::yield_cps(async:::R(4))))())

  cps_translate(quo({nseval::yield(1); base::yield(1); async::yield(1)}),
                gen_endpoints, local=FALSE) %is%
    quo(`{_cps`(async:::R(nseval::yield(1)), async:::yield_cps(async:::R(1)),
                async:::yield_cps(async:::R(1))))

  expect_equal(
    expr(cps_translate(quo(for (i in 1:10) {yield(i); base::`break`()}),
                       gen_endpoints, local=FALSE)),
    quote(for_cps(async:::R(i), async:::R(1:10), `{_cps`(
      yield_cps(async:::R(i)),
      async:::break_cps()))))

  cps_translate(quo(async::`if`(2 %% 5 == 0, yield(TRUE), yield(FALSE))),
                gen_endpoints, local=FALSE) %is%
    quo(async:::if_cps(async:::R(2%%5 == 0), yield_cps(async:::R(TRUE)),
                       yield_cps(async:::R(FALSE))))
})

test_that("leave functions and nested generators alone", {
  cps_translate(quo(for (i in gen(for (j in 1:10) yield(j))) yield(i)),
                endpoints = gen_endpoints, local=FALSE) %is%
    quo(for_cps(async:::R(i),
                async:::R(gen(for (j in 1:10) yield(j))),
                yield_cps(async:::R(i))))
})

test_that("Translating expressions", {
  xin <- quo({
    i <- 0
    repeat {
      i <- i + 1;
      if (i %% skip == 0) next
      if (i > max) break
      yield(i)
    }
  })

  xout <- quote(
    `{_cps`(
      async:::R(i <- 0),
      repeat_cps(
        `{_cps`(
          async:::R(i <- i + 1),
          if_cps(async:::R(i %% skip == 0),
                 next_cps()),
          if_cps(async:::R(i > max),
                 break_cps()),
          yield_cps(async:::R(i))
          ))))

  expect_equal(expr(
    cps_translate(
      xin, endpoints=c(base_endpoints, "yield"), local=FALSE)),
    xout)
})

test_that("Makes fully qualified names when async package not attached", {
  if ("package:async" %in% search()) {
    on.exit({
      attachNamespace("async")
    }, add=TRUE)
    detach("package:async")
  }
  xin <- nseval::quo({
    max <- 10
    skip <- 4
    i <- 0;
    repeat {
      i <- i + 1;
      if (i %% skip == 0) next
      if (i > max) break
      yield(i)
    }
  }, globalenv())

  target <- quote(
    (function()
    async:::`{_cps`(
      async:::R(max <- 10),
      async:::R(skip <- 4),
      async:::R(i <- 0),
      async:::repeat_cps(
        async:::`{_cps`(
        async:::R(i <- i + 1),
        async:::if_cps(
          async:::R(i %% skip == 0),
          async:::next_cps()),
        async:::if_cps(
          async:::R(i > max),
          async:::break_cps()),
        async:::yield_cps(async:::R(i)))))
    )())

  xout <- async:::cps_translate(xin, endpoints=c("yield", "next", "break"))
  expect_equal(nseval::expr(xout), target)

  # can run a generator without having the package attached
  g <- nseval::do(async::gen, xin)
  l <- as.numeric(as.list((g)))
  l %is% c(1, 2, 3, 5, 6, 7, 9, 10)
})

test_that("splitting pipes", {
  expect_error(cps_translate(quo( await(x)+2 ), endpoints=async_endpoints),
               "split_pipes")

  expect_identical(
    cps_translate(quo( await(x)+2 )
                , endpoints=async_endpoints, split_pipes=TRUE),
    cps_translate(quo( {..async.tmp <- await(x); ..async.tmp + 2} ),
                  endpoints=async_endpoints, split_pipes=FALSE))

  expect_error(
    cps_translate(quo( cat("first argument", yield(5)) ),
                  endpoints=gen_endpoints, split_pipes=TRUE),
    "pausable")
})

test_that("Nested split pipes", {
  # The head can be carried out of a nested call.
  expect_identical(
    cps_translate(quo(
      sort(await(open(with(await(directory), find_record(idCol)))))
    ), endpoints=async_endpoints, split_pipes=TRUE),
    cps_translate(quo({
      ..async.tmp <- await({
        ..async.tmp <- await(directory);
        open(with(..async.tmp, find_record(idCol)))
      });
      sort(..async.tmp)
    }), endpoints=async_endpoints, split_pipes=FALSE))

  ## expect_identical(
  ##   #actually should be identical to the prev test, but written
  ##   #all-left-to-right style which may be easier to follow.
  ##   expr(cps_translate(quo(
  ##     directory |>
  ##     await() |>
  ##     with(findRecord(idCol)) |>
  ##     open() |>
  ##     await() |>
  ##     sort()
  ##   ), endpoints=async_endpoints, split_pipes=TRUE)),
  ##   # the placement of braces winds up like this, but this aspect is
  ##   # not important as braces disappear when constructing the graph.
  ##   # Also note that if you only do this "splitting" to one
  ##   # argument, you should only need one temp var.
  ##   expr(cps_translate(quo(
  ##   {{directory |>
  ##     await() -> ..async.tmp; ..async.tmp |>
  ##     with(findRecord(idCol)) |>
  ##     open() } |>
  ##     await() -> ..async.tmp; ..async.tmp |>
  ##     sort() }
  ##   ), endpoints=async_endpoints, split_pipes=FALSE)))
})

test_that("Split pipe vs namespaces", {
  if ("package:async" %in% search()) {
    on.exit({
      attachNamespace("async")
    }, add=TRUE)
    detach("package:async")
  }

  locate("{_cps")
  x <- nseval::quo( await(x)+2 , baseenv() )
  t <- expr(async:::cps_translate(x, endpoints=async_endpoints,
                                  split_pipes=TRUE, local=FALSE))

  expect_equal(t, quote(
    async:::`{_cps`(
      async:::`<-_cps`(async:::R(..async.tmp), 
                       async:::await_cps(async:::R(x))),
      async:::R(..async.tmp + 2))))
})

test_that("Call in call head", {
  # wrapping a keyword call in parens makes us ignore it
  with(list(yield=function(x) x+5,
            yield_cps = async:::yield_cps),
  {
    cps_translate(quo( yield((yield)(5))),
                  endpoints=gen_endpoints, local=FALSE) %is%
      quo( yield_cps(async:::R((yield)(5))) )
    expect_error(
      cps_translate(quo( (yield)(yield(5))),
                    endpoints=gen_endpoints, local=FALSE),
      "pausable")

    # do we want to allow (await(getCallback()))(moreArgs) via pipe splitting?
    # I think you can instead await(getCallback()) |> callback(moreArgs), where
    # callback <- function(...) do_(get_dots())
    expect_error(
      cps_translate(quo( await(getCallback())(moreArgs) ),
                    endpoints=async_endpoints),
      "head")
  })
})

test_that("weird calls", {
  expect_error(expr(cps_translate(quo( 10(return(5))), local=FALSE)),
               "pausable")
  expect_error(expr(cps_translate(quo( NULL(return(5))), local=FALSE)),
               "pausable")

  notafunction <- "not a function"
  expect_error(expr(cps_translate(quo( notafunction(return(5))), local=FALSE)),
               "found")
})

test_that("find pausables", {
  p <- function(x) {
    dummy <- function() {1}
    dummy_cps <- function() function(cont, ...) function() cont(1)
    pausables(packages="async")
  }
  x <- p()
  # test that x contains....
  length(unique(c(alist(dummy), x))) %is% length(x)
  length(unique(c(alist(async::yield), x))) %is% length(x)
})
