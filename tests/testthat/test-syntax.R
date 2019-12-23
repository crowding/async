#' @import nseval

test_that("basic translations", {
  expect_warning(cps_translate(quo(x)) %is% quo(arg_cps(x)), "keywords")
  cps_translate(quo(break)) %is% quo(break_cps())
  expect_warning(cps_translate(quo(`break`)) %is% quo(arg_cps(`break`)), "keywords")
  bonk_cps <- function()function() NULL
  cps_translate(quo(bonk()), endpoints=c("bonk")) %is%quo(bonk_cps())
  cps_translate(quo(next)) %is% quo(next_cps())
  expect_error(cps_translate(quo(break())), "break")
  expect_warning(cps_translate(quo(2+2)) %is% quo(arg_cps(2+2)), "keywords")
  expect_error(cps_translate(quo(list(`break`(4)))), "CPS")
  cps_translate(endpoints="yield", quo(if(TRUE) yield(2+2) else yield(4))) %is%
    quo(if_cps(arg_cps(TRUE), yield_cps(arg_cps(2 + 2)), yield_cps(arg_cps(4))))

  expect_error(cps_translate(quo(flibbert(yield(5))), endpoints="yield"), "flibbert")
  expect_error(cps_translate(quo(rbind(yield(5))), endpoints="yield"), "rbind")
})

test_that("Namespace qualification", {
  # get an environment that doesn't see into nseval, even in testing

  cps_translate(quo(repeat generators::yield(4)), yield_endpoints) %is%
    quo(repeat_cps(generators:::yield_cps(arg_cps(4))))

  cps_translate(quo(base::`repeat`(generators::yield(4))), yield_endpoints) %is%
    quo(generators:::repeat_cps(generators:::yield_cps(arg_cps(4))))

  cps_translate(quo({nseval::yield(1); base::yield(1); generators::yield(1)}), yield_endpoints) %is%
    quo(`{_cps`(arg_cps(nseval::yield(1)), generators:::yield_cps(arg_cps(1)), 
                generators:::yield_cps(arg_cps(1))))

  expect_equal(
    expr(cps_translate(quo(for (i in 1:10) {yield(i); base::`break`()}),
                       yield_endpoints)) ,
    quote(for_cps(arg_cps(i), arg_cps(1:10), `{_cps`(
      yield_cps(arg_cps(i)),
      generators:::break_cps()))))

  cps_translate(quo(generators::`if`(2 %% 5 == 0, yield(TRUE), yield(FALSE))),
                yield_endpoints) %is%
    quo(generators:::if_cps(arg_cps(2%%5 == 0), yield_cps(arg_cps(TRUE)),
                             yield_cps(arg_cps(FALSE))))
})

test_that("leave functions and nested generators alone", {
  cps_translate(quo(for (i in gen(for (j in 1:10) yield(j))) yield(i)),
                    endpoints = yield_endpoints) %is%
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
                                  endpoints=c(base_endpoints, "yield"))),
               xout)
})

test_that("Fully qualified name references from a namespace not importing?", {
  # how to make package namespace not attached when using ESS tools?
  xin <- quo({
    i <- 0;
    repeat {
      i <- i + 1;
      if (i %% skip == 0) next()
      if (i > max) break
      yield(i)
    }
  }, getNamespace("nseval"))

  xout <- quote(
    generators:::`{_cps`(
      generators:::arg_cps(i <- 0),
      generators:::`{_cps`(
        generators:::arg_cps(i <- i + 1),
        generators::if_cps(
          i %% skip == 0,
          generators:::next_cps(), ),
        generators:::if_cps(
          generators:::arg_cps(i %% skip == 0),
          generators:::next_cps()),
        generators:::if_cps(
          generators:::arg_cps(i > max),
          generators:::break_cps()),
        generators:::yield_cps(i),
        )))
})
