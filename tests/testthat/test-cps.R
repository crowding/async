#' @import nseval

`%is%` <- expect_equal

test_that("arg_cps captures lazy arg", {
  f <- function(x) {arg_expr(x)}
  cp <- arg_cps(x+y)(f)
  cp() %is% quote(x+y)
})

test_that("arg_cps causes scoped effects,", {
  x <- 10
  f <- function(x) {
    arg_cps(x <- x + 1)
  }
  arg <- f(5)
  arg(function(val) val %is% 6)() %is% 6
  arg(function(val) val %is% 7)() %is% 7
  x %is% 10
})

test_that("arg_cps propagates errors", {
  arg_cps(10)(function(val) force(val))() %is% 10
  expect_error(arg_cps(stop("yes"))(function(val) force(val))(), "yes")
  arg_cps(10+20)(function(val) val)() %is% 30
  expect_error(arg_cps(stop("yes"))(function(val) val)(), "yes")
  # there is a quirk here where "stop()" is given as the argument to
  # an arg_cps(), and options(error=recover) is also set.
  # The promise caontining "stop()" is evaluated again, during the stack
  # readout?
  # This also happens when testthat is trying to gather stack trace from a failure
  # Perhaps I should stick a browser() statement inside of the arg to arg_cps.
  # Or, I should catch and rethrow in pump(). But that removes all
  # inspectability from the process.
})

test_that("()", {
  pump(`(_cps`(arg_cps(12+12))) %is% 24
  expect_error(pump(`(_cps`(arg_cps(stop("yes")))), "yes")
  expect_error(pump(`(_cps`(arg_cps({stop("yes")}))), "yes")
})

test_that("||", {
  pump(`||_cps`(arg_cps(FALSE), arg_cps(0))) %is% FALSE
  pump(`||_cps`(arg_cps(NA), arg_cps(0))) %is% NA
  pump(`||_cps`(arg_cps(TRUE), arg_cps(stop("no")))) %is% TRUE
  pump(`||_cps`(arg_cps(0), arg_cps(1))) %is% TRUE
  pump(`||_cps`(arg_cps(FALSE), arg_cps(NA))) %is% NA
  expect_error(pump(`||_cps`(arg_cps(FALSE), arg_cps(stop("yes")))), "yes")
})

test_that("&&", {
  pump(`&&_cps`(arg_cps(FALSE), arg_cps(stop("no")))) %is% FALSE
  pump(`&&_cps`(arg_cps(TRUE), arg_cps(FALSE))) %is% FALSE
  pump(`&&_cps`(arg_cps(NA), arg_cps(TRUE))) %is% NA
  pump(`&&_cps`(arg_cps(1), arg_cps(1))) %is% TRUE
  pump(`&&_cps`(arg_cps(FALSE), arg_cps(stop("yes")))) %is% FALSE
  expect_error(pump(`&&_cps`(arg_cps(TRUE), arg_cps(stop("yes")))), "yes")
  expect_error(pump(`&&_cps`(arg_cps(stop("yes")), arg_cps(FALSE))), "yes")
})

test_that("if", {
  pump(if_cps(arg_cps(3 > 2), arg_cps("left"), arg_cps("right"))) %is% "left"  
  pump(if_cps(arg_cps(2 > 3), arg_cps("left"), arg_cps("right"))) %is% "right"
  pump(if_cps(arg_cps(2 > 3), arg_cps("left"))) %is% NULL
#  pump(if_cps(arg_cps(2 > 3), arg_cps("left"), arg_cps())) %is% NULL
  expect_error(pump(if_cps(arg_cps(2 < 3), arg_cps(stop("no")))), "no")
  expect_error(pump(if_cps(arg_cps(stop("no")), arg_cps(2 < 3))), "no")
})

test_that("<-", {
  pump(`<-_cps`(arg_cps(x), arg_cps(5))) %is% 5
  x %is% 5
  pump(`<-_cps`(arg_cps(x), arg_cps(x + 1))) %is% 6
  x %is% 6
  (function(x) {
    pump(`<<-_cps`(arg_cps(x), arg_cps(x+1)))
  })(12) %is% 13
  x %is% 13
})

test_that("{}", {
  pump(`{_cps`()) %is% NULL
  expect_output(pump(`{_cps`(arg_cps(cat("hello\n")))), "hello") %is% NULL
  pump(`{_cps`(arg_cps(x <- 10))) %is% 10
  x %is% 10
  pump(`{_cps`(arg_cps(5))) %is% 5
  pump(`{_cps`(arg_cps(x <- 5),
               arg_cps(x <- x+4))) %is% 9
  expect_error(pump(`{_cps`(arg_cps(5), )), "missing")
  expect_error(pump(`{_cps`(arg_cps())), "missing")
  expect_error(pump(`{_cps`(arg_cps(5), arg_cps())), "missing")
})

test_that("repeat", {
  pump(repeat_cps(break_cps())) %is% NULL
  x <- 0
  cps <- repeat_cps(`{_cps`(arg_cps(x <- x + 1),
                            if_cps(arg_cps(x > 5), break_cps())))
  pump(cps)
  x %is% 6
  out <- c()
  cps <- repeat_cps(`{_cps`(arg_cps(x <- x - 1),
                            if_cps(arg_cps(x %% 2 == 0),
                                   next_cps(),
                                   arg_cps(out <- c(out, x))),
                            if_cps(arg_cps(x <= 0), break_cps())))
  pump(cps)
  out %is% c(5, 3, 1, -1)
})

test_that("while", {
  pump(while_cps(arg_cps(TRUE), break_cps())) %is% NULL

  x <- 0
  cps <- while_cps(arg_cps(x < 5), arg_cps(x <- x + 1))
  pump(cps)
  x %is% 5

  out <- c()
  x <- 6
  cps <- while_cps(
    arg_cps(x > 0),
    `{_cps`(arg_cps(x <- x - 1),
            if_cps(arg_cps(x %% 2 == 0),
                   next_cps(),
                   arg_cps(out <- c(out, x)))))
  pump(cps)
  out %is% c(5, 3, 1)
})

test_that("for", {
  x <- 0
  pump(for_cps(arg_cps(i), arg_cps(1:10), arg_cps({x <- x + i})))
  x %is% 55
  #
  x <- 0
  pump(for_cps(arg_cps(i),
               arg_cps(1:10),
               `{_cps`(
                 arg_cps(force(i)),
                 if_cps(arg_cps(i %% 3 == 0), next_cps()),
                 if_cps(arg_cps(i %% 8 == 0), break_cps()),
                 arg_cps({x <- x + i}))))
  x %is% 19 # 1 + 2 + 4 + 5 + 7
  i %is% 8
  #
  out <- c()
  x <- 6
  cps <- while_cps(
    arg_cps(x > 0),
    `{_cps`(arg_cps(x <- x - 1),
            if_cps(arg_cps(x %% 2 == 0),
                   next_cps(),
                   arg_cps(out <- c(out, x)))))
  pump(cps)
  out %is% c(5, 3, 1)
})

test_that("pump forces value or error", {
  pump(arg_cps(12+12)) %is% 24
  expect_error(pump(arg_cps(stop("yes"))), "yes")
  expect_error(pump(arg_cps()), "missing")
})
