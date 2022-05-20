#' @import nseval

`%is%` <- expect_equal

test_that("R captures lazy arg", {
  f <- function(x) {arg_expr(x)}
  cp <- R(x+y)(f)
  cp() %is% quote(x+y)
})

test_that("R causes scoped effects,", {
  x <- 10
  f <- function(x) {
    R(x <- x + 1)
  }
  arg <- f(5)
  arg(function(val) val %is% 6)() %is% 6
  arg(function(val) val %is% 7)() %is% 7
  x %is% 10
})

test_that("R propagates errors", {
  R(10)(function(val) force(val))() %is% 10
  expect_error(R(stop("yes"))(function(val) force(val))(), "yes")
  R(10+20)(function(val) val)() %is% 30
  expect_error(R(stop("yes"))(function(val) val)(), "yes")
  # there is a quirk here where "stop()" is given as the argument to
  # an R(), and options(error=recover) is also set.
  # The promise caontining "stop()" is evaluated again, during the stack
  # readout?
  # This also happens when testthat is trying to gather stack trace from a failure
  # Perhaps I should stick a browser() statement inside of the arg to R.
  # Or, I should catch and rethrow in pump(). But that removes all
  # inspectability from the process.
})

test_that("()", {
  pump(`(_cps`(R(12+12))) %is% 24
  expect_error(pump(`(_cps`(R(stop("yes")))), "yes")
  expect_error(pump(`(_cps`(R({stop("yes")}))), "yes")
})

test_that("||", {
  pump(`||_cps`(R(FALSE), R(0))) %is% FALSE
  pump(`||_cps`(R(NA), R(0))) %is% NA
  pump(`||_cps`(R(TRUE), R(stop("no")))) %is% TRUE
  pump(`||_cps`(R(0), R(1))) %is% TRUE
  pump(`||_cps`(R(FALSE), R(NA))) %is% NA
  expect_error(pump(`||_cps`(R(FALSE), R(stop("yes")))), "yes")
})

test_that("&&", {
  pump(`&&_cps`(R(FALSE), R(stop("no")))) %is% FALSE
  pump(`&&_cps`(R(TRUE), R(FALSE))) %is% FALSE
  pump(`&&_cps`(R(NA), R(TRUE))) %is% NA
  pump(`&&_cps`(R(1), R(1))) %is% TRUE
  pump(`&&_cps`(R(FALSE), R(stop("yes")))) %is% FALSE
  expect_error(pump(`&&_cps`(R(TRUE), R(stop("yes")))), "yes")
  expect_error(pump(`&&_cps`(R(stop("yes")), R(FALSE))), "yes")
})

test_that("if", {
  pump(if_cps(R(3 > 2), R("left"), R("right"))) %is% "left"
  pump(if_cps(R(2 > 3), R("left"), R("right"))) %is% "right"
  pump(if_cps(R(2 > 3), R("left"))) %is% NULL
  #pump(if_cps(R(2 > 3), R("left"), R())) %is% NULL
  expect_error(pump(if_cps(R("notalogical"), R(1), R(2))))
  expect_error(pump(if_cps(R(2 < 3), R(stop("no")))), "no")
  expect_error(pump(if_cps(R(stop("no")), R(2 < 3))), "no")
})

test_that("<-", {
  pump(`<-_cps`(R(x), R(5))) %is% 5
  x %is% 5
  pump(`<-_cps`(R(x), R(x + 1))) %is% 6
  x %is% 6
  (function(x) {
    pump(`<<-_cps`(R(x), R(x+1)))
  })(12) %is% 13
  x %is% 13
  pump(`<-_cps`(R(x[2]), R(5))) %is% 5
  x %is% c(13, 5)
})

test_that("{}", {
  pump(`{_cps`()) %is% NULL
  expect_output(pump(`{_cps`(R(cat("hello\n")))), "hello") %is% NULL
  pump(`{_cps`(R(x <- 10))) %is% 10
  x %is% 10
  pump(`{_cps`(R(5))) %is% 5
  pump(`{_cps`(R(x <- 5), R({x <- x+4}))) %is% 9
  x %is% 9
  expect_error(pump(`{_cps`(R(5), )), "missing")
  expect_error(pump(`{_cps`(R())), "(missing|found)")
  expect_error(pump(`{_cps`(R(5), R())), "(missing|found)")
})

test_that("repeat", {
  pump(repeat_cps(break_cps())) %is% NULL
  x <- 0
  cps <- repeat_cps(`{_cps`(R(x <- x + 1),
                            if_cps(R(x > 5), break_cps())))
  pump(cps)
  x %is% 6
  out <- c()
  cps <- repeat_cps(`{_cps`(R(x <- x - 1),
                            if_cps(R(x %% 2 == 0),
                                   next_cps(),
                                   R(out <- c(out, x))),
                            if_cps(R(x <= 0), break_cps())))
  pump(cps)
  out %is% c(5, 3, 1, -1)

  expect_error(pump(if_cps(R(TRUE), break_cps(), R(2))), "break")
  expect_error(pump(if_cps(R(TRUE), next_cps(), R(2))), "next")
})

test_that("while", {
  pump(while_cps(R(TRUE), break_cps())) %is% NULL
  pump(while_cps(break_cps(), R(TRUE))) %is% NULL

  x <- 0
  cps <- while_cps(R(x < 5), R(x <- x + 1))
  pump(cps)
  x %is% 5

  out <- c()
  x <- 6
  cps <- while_cps(
    R(x > 0),
    `{_cps`(R(x <- x - 1),
            if_cps(R(x %% 2 == 0),
                   next_cps(),
                   R(out <- c(out, x)))))
  pump(cps)
  out %is% c(5, 3, 1)
})

test_that("for", {
  x <- 0
  pump(for_cps(R(i), R(1:10), R({x <- x + i})))
  x %is% 55
  #
  x <- 0
  pump(for_cps(R(i),
               R(1:10),
               `{_cps`(
                 R(force(i)),
                 if_cps(R(i %% 3 == 0), next_cps()),
                 if_cps(R(i %% 8 == 0), break_cps()),
                 R({x <- x + i}))))
  x %is% 19 # 1 + 2 + 4 + 5 + 7
  i %is% 8
  #
  out <- c()
  x <- 6
  cps <- for_cps(
    R(x),
    R(1:10),
    if_cps(R(x %% 2 == 0),
           next_cps(),
           R(out <- c(out, x))))
  pump(cps)
  out %is% c(1, 3, 5, 7, 9)
})

test_that("for over iterator", {
  x <- 0
  pump(for_cps(R(i), R(icount(10)), R(x <- x + i)))
  x %is% 55
})

test_that("pump forces value or error", {
  pump(R(12+12)) %is% 24
  expect_error(pump(R(stop("yes"))), "yes")
  expect_error(pump(R()), "(missing|found)")
})

test_that("switch", {
  expect_null(pump(switch_cps(R("three"), four=R("wrong"))))
  pump(switch_cps(R("three"), four=R("wrong"), three=R("right"))) %is% "right"
  expect_error(
    pump(switch_cps(R("C"), R(0), R(0), three=R(0))),
    "default")
  pump(switch_cps(R(3),
                  ignored=R(stop()),
                  R(stop()),
                  R(5),
                  R(stop()))) %is% 5
  pump(switch_cps(R("default"), special=R(22), R(33))) %is% 33
})
