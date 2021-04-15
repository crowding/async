#' @import nseval
`%is%` <- expect_equal
all.equal.quotation <- all.equal.function

test_that("basic translations", {
  expect_warning(cps_translate(quo(x), local=FALSE) %is% quo(arg_cps(x)), "keywords")
  cps_translate(quo(break), local=FALSE) %is% quo(break_cps())
  expect_warning(cps_translate(quo(`break`), local=FALSE) %is% quo(arg_cps(`break`)), "keywords")
  bonk_cps <- function()function() NULL
  cps_translate(quo(bonk()), endpoints=c("bonk"), local=FALSE) %is% quo(bonk_cps())
  cps_translate(quo(next), local=FALSE) %is% quo(next_cps())
  expect_error(cps_translate(quo(break())), "break")
  expect_warning(cps_translate(quo(2+2), local=FALSE) %is% quo(arg_cps(2+2)), "keywords")
  expect_error(cps_translate(quo(list(`break`(4)))), "CPS")
  cps_translate(endpoints="yield",
                quo(if(TRUE) yield(2+2) else yield(4)), local=FALSE) %is%
    quo(if_cps(arg_cps(TRUE), yield_cps(arg_cps(2 + 2)), yield_cps(arg_cps(4))))

  expect_error(cps_translate(quo(flibbert(yield(5))), endpoints="yield"), "flibbert")
  expect_error(cps_translate(quo(rbind(yield(5))), endpoints="yield"), "rbind")
})

test_that("Namespace qualification", {
  # get an environment that doesn't see into nseval, even in testing

  cps_translate(quo(repeat async::yield(4)), gen_endpoints) %is%
    quo((function() repeat_cps(async:::yield_cps(arg_cps(4))))())

  cps_translate(quo(base::`repeat`(async::yield(4))), gen_endpoints) %is%
    quo((function() async:::repeat_cps(async:::yield_cps(arg_cps(4))))())

  cps_translate(quo({nseval::yield(1); base::yield(1); async::yield(1)}),
                gen_endpoints, local=FALSE) %is%
    quo(`{_cps`(arg_cps(nseval::yield(1)), async:::yield_cps(arg_cps(1)),
                async:::yield_cps(arg_cps(1))))

  expect_equal(
    expr(cps_translate(quo(for (i in 1:10) {yield(i); base::`break`()}),
                       gen_endpoints, local=FALSE)),
    quote(for_cps(arg_cps(i), arg_cps(1:10), `{_cps`(
      yield_cps(arg_cps(i)),
      async:::break_cps()))))

  cps_translate(quo(async::`if`(2 %% 5 == 0, yield(TRUE), yield(FALSE))),
                gen_endpoints, local=FALSE) %is%
    quo(async:::if_cps(arg_cps(2%%5 == 0), yield_cps(arg_cps(TRUE)),
                             yield_cps(arg_cps(FALSE))))
})

test_that("leave functions and nested generators alone", {
  cps_translate(quo(for (i in gen(for (j in 1:10) yield(j))) yield(i)),
                    endpoints = gen_endpoints, local=FALSE) %is%
    quo(for_cps(arg_cps(i),
                arg_cps(gen(for (j in 1:10) yield(j))),
                yield_cps(arg_cps(i))))
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
      arg_cps(i <- 0),
      repeat_cps(
        `{_cps`(
          arg_cps(i <- i + 1),
          if_cps(arg_cps(i %% skip == 0),
                 next_cps()),
          if_cps(arg_cps(i > max),
                 break_cps()),
          yield_cps(arg_cps(i))
          ))))

  expect_equal(expr(cps_translate(xin,
                                  endpoints=c(base_endpoints, "yield"),
                                  local=FALSE)),
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
      async:::arg_cps(max <- 10),
      async:::arg_cps(skip <- 4),
      async:::arg_cps(i <- 0),
      async:::repeat_cps(
        async:::`{_cps`(
        async:::arg_cps(i <- i + 1),
        async:::if_cps(
          async:::arg_cps(i %% skip == 0),
          async:::next_cps()),
        async:::if_cps(
          async:::arg_cps(i > max),
          async:::break_cps()),
        async:::yield_cps(async:::arg_cps(i)))))
    )())

  xout <- async:::cps_translate(xin, endpoints=c("yield", "next", "break"))
  expect_equal(nseval::expr(xout), target)

  # can run a generator without having the package attached
  g <- nseval::do(async::gen, xin)
  l <- as.numeric(as.list((g)))
  l %is% c(1, 2, 3, 5, 6, 7, 9, 10)
})
