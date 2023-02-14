#' @import nseval

`%is%` <- expect_equal

# Due to passing all the handlers down in named arguments, it's easy
# to drop one on the floor and forget to pass it along to child
# functions.  So here's a static check that node constructors pass
# down all the named args after ... that they receive.
test_that("package consistency check: passing named handlers", {

  `%--%` <- function(from, to) seq_len(to-from+1) + (from-1)

  find_constructor <- function(expr) {
    switch(
      typeof(expr),
      language={
        if (is.name(expr[[1]])
            && as.character(expr[[1]]) == "function") {
          formals <- expr[[2]]
          if ("..." %in% names(formals)) {
            ix <- which(names(formals) == "...")[[1]]
            handlers <- names(formals)[(ix+1) %--% length(formals)]
            find_constructor_calls(expr[[3]], handlers)
          }
        } else {
          for (i in as.list(expr)) {
            if (!missing(i))
              find_constructor(i)
          }
        }
      },
      NULL
    )
  }

  find_constructor_calls <- function(expr, handlers) {
    force(expr)
    switch(
      typeof(expr),
      language={
        isdots <- vapply(expr, identical, FALSE, quote(...))
        if (any(isdots)) {
          ix <- which(isdots)[[1]]
          names <- names(expr[ (ix+1) %--% length(expr) ])
          missing <- setdiff(handlers, names)
          if (length(missing) > 0) {
            stop("handler(s) ", deparse(missing), " missing from call ", deparse(expr))
          }
          expect_true(all(handlers %in% names))
        } else {
          for (i in as.list(expr)) {
            if (!missing(i))
              find_constructor_calls(i, handlers)
          }
        }
      },
      NULL
    )
  }

  env <- getNamespace("async")
  names <- grep("_cps$", names(env), value=TRUE)
  for (name in names) {
    f <- get(paste0(as.character(name, "_cps")), env)
    find_constructor(body(f))
  }

})

test_that("eval handler determines R scope,", {

  e <- environment()
  evl <- \(cont, val) cont(eval(val,e))
  evl2 <- NULL
  x <- 10
  f <- function(x) {
    e <- environment()
    evl2 <<- \(cont, val) cont(eval(val,e))
    R("", x <- x + 1)
  }
  arg <- f(5)
  arg(\(val) val %is% 11, evl=evl)() %is% 11
  arg(\(val) val %is% 6, evl=evl2)() %is% 6
  x %is% 11

})

evl_ <- \(cont, val) cont(eval(val,environment(evl_)))

test_that("R propagates errors", {
  R("", 10)(function(val) force(val), evl=evl_)() %is% 10
  expect_error(R("", stop("yes"))(function(val) force(val), evl=evl_)(), "yes")
  R("", 10+20)(function(val) val, evl=evl_)() %is% 30
  expect_error(R("", stop("yes"))(function(val) val, evl=evl_)(), "yes")
})

test_that("()", {
  pump(`(_cps`("", R("", 12+12)), targetEnv=environment()) %is% 24
  expect_error(pump(`(_cps`("", R("", stop("yes"))), targetEnv=environment()), "yes")
  expect_error(pump(`(_cps`("", R("", {stop("yes")})), targetEnv=environment()), "yes")
})

test_that("||", {
  pump(`||_cps`("", R("", FALSE), R("", 0)), targetEnv=environment()) %is% FALSE
  pump(`||_cps`("", R("", NA), R("", 0)), targetEnv=environment()) %is% NA
  pump(`||_cps`("", R("", TRUE), R("", stop("no"))), targetEnv=environment()) %is% TRUE
  pump(`||_cps`("", R("", 0), R("", 1)), targetEnv=environment()) %is% TRUE
  pump(`||_cps`("", R("", FALSE), R("", NA)), targetEnv=environment()) %is% NA
  expect_error(pump(`||_cps`("", R("", FALSE), R("", stop("yes"))), targetEnv=environment()), "yes")
})

test_that("&&", {
  pump(`&&_cps`("", R("", FALSE), R("", stop("no"))), targetEnv=environment()) %is% FALSE
  pump(`&&_cps`("", R("", TRUE), R("", FALSE)), targetEnv=environment()) %is% FALSE
  pump(`&&_cps`("", R("", NA), R("", TRUE)), targetEnv=environment()) %is% NA
  pump(`&&_cps`("", R("", 1), R("", 1)), targetEnv=environment()) %is% TRUE
  pump(`&&_cps`("", R("", FALSE), R("", stop("yes"))), targetEnv=environment()) %is% FALSE
  expect_error(pump(`&&_cps`("", R("", TRUE), R("", stop("yes"))), targetEnv=environment()), "yes")
  expect_error(pump(`&&_cps`("", R("", stop("yes")), R("", FALSE)), targetEnv=environment()), "yes")
})

test_that("if", {
  pump(if_cps("", R("", 3 > 2), R("", "left"), R("", "right")), targetEnv=environment()) %is% "left"
  pump(if_cps("", R("", 2 > 3), R("", "left"), R("", "right")), targetEnv=environment()) %is% "right"
  pump(if_cps("", R("", 2 > 3), R("", "left")), targetEnv=environment()) %is% NULL
  #pump(if_cps("", R("", 2 > 3), R("", "left"), R("")), targetEnv=environment()) %is% NULL
  expect_error(pump(if_cps("", R("", "notalogical"), R("", 1), R("", 2)), targetEnv=environment()))
  expect_error(pump(if_cps("", R("", 2 < 3), R("", stop("no"))), targetEnv=environment()), "no")
  expect_error(pump(if_cps("", R("", stop("no")), R("", 2 < 3)), targetEnv=environment()), "no")
})

test_that("<-", {
  pump(`<-_cps`("", R("", x), R("", 5)),
       targetEnv=environment()) %is% 5
  x %is% 5
  pump(`<-_cps`("", R("", x), R("", x + 1)),
       targetEnv=environment()) %is% 6
  x %is% 6
  (function(x) {
    pump(`<<-_cps`("", R("", x), R("", x+1)),
         targetEnv=environment())
  })(12) %is% 13
  x %is% 13
  pump(`<-_cps`("", R("", x[2]), R("", 5)),
       targetEnv=environment()) %is% 5
  x %is% c(13, 5)
})

test_that("{}", {
  pump(`{_cps`(""), targetEnv=environment()) %is% NULL
  expect_output(pump(`{_cps`("", R("", cat("hello\n"))), targetEnv=environment()), "hello") %is% NULL
  pump(`{_cps`("", R("", x <- 10)), targetEnv=environment()) %is% 10
  x %is% 10
  pump(`{_cps`("", R("", 5)), targetEnv=environment()) %is% 5
  pump(`{_cps`("", R("", x <- 5), R("", {x <- x+4})), targetEnv=environment()) %is% 9
  x %is% 9
  expect_error(pump(`{_cps`("", R("", 5), ), targetEnv=environment()), "missing")
  expect_error(pump(`{_cps`("", R("")), targetEnv=environment()), "(missing|found)")
  expect_error(pump(`{_cps`("", R("", 5), R("")), targetEnv=environment()), "(missing|found)")
})

test_that("repeat", {
  pump(repeat_cps("", break_cps("")), targetEnv=environment()) %is% NULL
  x <- 0
  cps <- repeat_cps("", `{_cps`("", R("", x <- x + 1),
                            if_cps("", R("", x > 5), break_cps(""))))
  pump(cps, targetEnv=environment())
  x %is% 6
  out <- c()
  cps <- repeat_cps("", `{_cps`("", R("", x <- x - 1),
                            if_cps("", R("", x %% 2 == 0),
                                   next_cps(""),
                                   R("", out <- c(out, x))),
                            if_cps("", R("", x <= 0), break_cps(""))))
  pump(cps, targetEnv=environment())
  out %is% c(5, 3, 1, -1)

  expect_error(pump(if_cps("", R("", TRUE), break_cps(""), R("", 2)),
                    targetEnv=environment()), "break")
  expect_error(pump(if_cps("", R("", TRUE), next_cps(""), R("", 2)),
                    targetEnv=environment()), "next")
})

test_that("while", {
  pump(while_cps("", R("", TRUE), break_cps("")),
       targetEnv=environment()) %is% NULL
  pump(while_cps("", break_cps(""), R("", TRUE)),
       targetEnv=environment()) %is% NULL

  x <- 0
  cps <- while_cps("", R("", x < 5), R("", x <- x + 1))
  pump(cps, targetEnv=environment())
  x %is% 5

  out <- c()
  x <- 6
  cps <- while_cps("",
    R("", x > 0),
    `{_cps`("", R("", x <- x - 1),
            if_cps("", R("", x %% 2 == 0),
                   next_cps(""),
                   R("", out <- c(out, x)))))
  pump(cps, targetEnv=environment())
  out %is% c(5, 3, 1)
})

test_that("for", {
  x <- 0
  pump(for_cps("", R("", i), R("", 1:10), R("", {x <- x + i})),
       targetEnv=environment())
  x %is% 55
  #

  x <- 0
  pump(for_cps("", R("", i),
               R("", 1:10),
               `{_cps`("",
                 R("", force(i)),
                 if_cps("", R("", i %% 3 == 0), next_cps("")),
                 if_cps("", R("", i %% 8 == 0), break_cps("")),
                 R("", {x <- x + i}))), targetEnv=environment())


  x %is% 19 # 1 + 2 + 4 + 5 + 7
  i %is% 8

  out <- c()
  x <- 6
  cps <- for_cps("",
    R("", x),
    R("", 1:10),
    if_cps("", R("", x %% 2 == 0),
           next_cps(""),
           R("", out <- c(out, x))))
  pump(cps, targetEnv=environment())
  out %is% c(1, 3, 5, 7, 9)

  expect_error(pump(for_cps("",
                            try_cps("", R("", i)),
                            R("", NULL),
                            R("", NULL)), targetEnv=environment()), "xpected")
  expect_error(pump(for_cps("", R("", 4),
                            R("", NULL),
                            R("", NULL)), targetEnv=environment()), "xpected")
})

test_that("for over iterator", {
  x <- 0
  pump(for_cps("", R("", i), R("", iseq(1,10)), R("", x <- x + i)),
       targetEnv=environment())
  x %is% 55
})

test_that("pump forces value or error", {
  pump(R("", 12+12), targetEnv=environment()) %is% 24
  expect_error(pump(R("", stop("yes")), targetEnv=environment()), "yes")
  expect_error(pump(R(""), targetEnv=environment()), "(missing|found)")
})

test_that("switch", {
  expect_error(pump(switch_cps("", R("", "three"), four=R("", "wrong")),
                    targetEnv=environment()), "branch")
  pump(switch_cps("", R("", "three"),
                  four=R("", "wrong"),
                  three=R("", "right")),
       targetEnv=environment()) %is% "right"
  expect_error(
    pump(switch_cps("", R("", "C"),
                    R("", 0),
                    R("", 0),
                    three=R("", 0)),
         targetEnv=environment()),
    "default")

  # variance from R: R will accept a numeric arg to a labeled `switch`
  switch(3, ignored=stop(), stop(), 5, stop()) %is% 5
  # Whereas async switch will expect either a character argument, if
  # labels are present, or a numeric one if they are not; this error
  # throws at compile time.
  expect_error(
    pump(switch_cps("", R("", 3),
                    ignored=R("", stop()),
                    R("", stop()),
                    R("", 5),
                    R("", stop())),
         targetEnv=environment()) %is% 5,
    "uplicate")

  pump(switch_cps("", R("", "default"), special=R("", 22), R("", 33)),
       targetEnv=environment()) %is% 33
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
    async:::`{_cps`(
      ".{",
      async:::R(".{1.R", max <- 10),
      async:::R(".{2.R", skip <- 4),
      async:::R(".{3.R", i <- 0),
      async:::repeat_cps(
        ".{4.repeat",
        async:::`{_cps`(
          ".{4.repeat.{",
          async:::R(".{4.repeat.{1.R", i <- i + 1),
          async:::if_cps(
            ".{4.repeat.{2.if",
            async:::R(".{4.repeat.{2.if1.R", i%%skip == 0),
            async:::next_cps(".{4.repeat.{2.if2.next")),
          async:::if_cps(
            ".{4.repeat.{3.if",
            async:::R(".{4.repeat.{3.if1.R", i > max),
            async:::break_cps(".{4.repeat.{3.if2.break")),
          async:::yield_cps(
            ".{4.repeat.{4.yield",
            async:::R(".{4.repeat.{4.yield.R", i))))))

  xout <- async:::cps_translate(xin, endpoints=c("yield", "next", "break"))

  expect_identical(expr(xout), target)

})

# becomes
# function(x, y) {list(x, y); async::gen(for (i in x) yield(i+y), local=FALSE)}
test_that("coroutine function transform", {

  x <- coroutine_function(quo(
    function(x, y) {
      for (i in x) yield(i+y)
    }),
    head=quote(async::gen),
    split_pipes=FALSE)

  x %is% quo(function(x, y) {
    list(x, y)
    async::gen({
        for (i in x) yield(i + y)
    }, local = FALSE, split_pipes=FALSE)
  })

  x <- coroutine_function(quo(\(...) for (i in list(...)) yield(i)),
                          head=quote(async::stream))

  x %is% quo(function(...) {
    list(...)
    async::stream(for (i in list(...)) yield(i), local = FALSE)
  })

})

