fn <- function() {
  x
}
setCompileLevelFromFn(fn)

test_that("on.exit", {

  closed <- FALSE
  close <- function() closed <<- TRUE
  run({
    on.exit({close()})
    expect_false(closed)
  }, debugInternal=FALSE)
  expect_true(closed)

  closed <- FALSE
  expect_error(
    run({
      on.exit(close())
      expect_false(closed)
      stop("stopping")
    }, debugInternal=FALSE)
  , "ping")
  expect_true(closed)

  closed <- FALSE
  expect_error(run({
    on.exit(close())
    expect_false(closed)
    yield(s0me+nons3nse)
  }), "s0me")
  expect_true(closed)

  r <- run({
    on.exit(yield("done!"))
    yieldFrom(1:3)
  })
  r %is% list(1, 2, 3, "done!")

})

test_that("on.exit in generator", {

  closed <- FALSE
  g <- gen({
    on.exit(closed <<- TRUE)
    yieldFrom(1:3)
  })
  nextOr(g, NULL) %is% 1
  nextOr(g, NULL) %is% 2
  nextOr(g, NULL) %is% 3
  expect_false(closed)
  nextOr(g, "done") %is% "done"
  expect_true(closed)

  closed <- FALSE
  g <- gen({
    on.exit(closed <<- TRUE)
    expect_false(closed)
    yield(s0me+nons3nse)
  })
  expect_error(nextOr(g, NULL), "s0me")
  expect_true(closed)

})

test_that("on.exit in async; can override", {

  p <- mock_promise()
  as <- async({
    on.exit(return("changed my mind!"))
    await(p)
  })
  expect_resolves_with(as, "changed my mind!", p$reject("sad"))

  p <- mock_promise()
  as <- async({
    on.exit(stop("changed my mind."))
    await(p)
  }, compileLevel=-1)
  expect_rejects_with(as, "changed my mind.", p$resolve("happy"))

  p <- mock_promise()
  debugAsync(as, internal=FALSE)
  as <- async({
    on.exit({
      return("actually happy!")
    })
    await(p) + (s0me+n0nsense)
  })
  expect_resolves_with(as, "actually happy!", p$resolve("sad"))

  # can also await in on.exit, but this propagates the error.
  p <- mock_promise()
  q <- mock_promise()
  as <- async({
    on.exit({
      paste0(await(q), "what?!")
    })
    await(p) + (s0me+n0nsense)
  })
  p$resolve("hello")
  expect_rejects_with(as, "s0me", q$resolve("wait"))

})

test_that("extremely bananas: simultaneously errors and returns", {

  # okay now, here is what R does:
  # you override a stop with a return
  r <- function() {
    on.exit(return("5!"))
    stop("this throws")
  }
  g <- NULL
  # expect_error sees an error:
  expect_error(g <- r(), "throws")
  # And yet, it returned a value.
  g %is% "5!"

  # Can "run" replicate this?
  g <- NULL
  expect_error(g <- run({
    on.exit({return(5)})
    for (i in 1:3) i
    stop("stopping")
    4
  }), "stopping")
  g %is% 5

  # what about a generator? okay look this is bananas
  g <- gen({
    on.exit({yield("done!")})
    yieldFrom(1:3)
    stop("stopping")
    yield(4)
  })
  nextOr(g, NULL) %is% 1
  nextOr(g, NULL) %is% 2
  nextOr(g, NULL) %is% 3
  x <- NULL
  expect_error(x <- nextOr(g, NULL), "stopping")
  # an error made its way to the top, and yet,
  x %is% "done!"
  # this kinda wrecks my mental model of R error handling tbh.
  # the fact that it _also_ works in a generator is bananas.
  expect_error(nextOr(g, NULL), "swallow")

})

test_that("on.exit error from tryCatch", {

  x <- NULL
  y <- NULL
  r <- run(function(){
    on.exit(x <<- "exited")
    tryCatch({stop("stoppp!"); 5}, finally={y <<- "finally"})
  })
  expect_error(r(), "stoppp")
  x %is% "exited"
  y %is% "finally"

  x <- NULL
  y <- NULL
  run({
    on.exit(x <- "exited")
    tryCatch({return("return"); 5}, finally={y <- "finally"})
  }) %is% "return"
  x %is% "exited"
  y %is% "finally"

})

test_that("on.exit before or after", {

  x <- 5
  run({
    on.exit(x <- x * 2)
    on.exit(x <- x + 3)
    TRUE
  })
  x %is% 8

  x <- 5
  run({
    on.exit(x <- x * 2)
    on.exit(x <- x + 3, add=TRUE)
    TRUE
  })
  x %is% 13

  x <- 5
  run({
    on.exit(x <- x * 2)
    on.exit(x <- x + 3, add=TRUE, after=FALSE)
    TRUE
  })
  x %is% 16

})

my_expect_error <- function(code, pattern) {
  # because the next test has code that throws multiple errors
  # which expect_that doesn't handle?
  tryCatch({
    code
    stop("did not stop")
  }, error=function(err) {
    if (!stringr::str_detect(conditionMessage(err), pattern)) {
      stop(err)
    }
  })
}

test_that("error in first on.exit does not prevent second", {

  one <- FALSE; two <- FALSE
  f <- function()({
    on.exit({one <<- TRUE; 1[[2]]})
    on.exit({two <<- TRUE; 4$foo}, add=TRUE)
    5+"duck"
  })
  my_expect_error(f(), "atomic")

  one <- FALSE; two <- FALSE
  f <- run(function() {
    on.exit({one <<- TRUE; 1[[2]]})
    on.exit({two <<- TRUE; 4$foo}, add=TRUE)
    5+"duck"
  })
  my_expect_error(f(), "atomic")
  expect_true(one && two)

  one <- FALSE; two <- FALSE
  mp <- mock_promise()
  as <- async({
    await(mp)
    on.exit({one <<- TRUE; 1[[2]]})
    on.exit({two <<- TRUE; 4$foo}, add=TRUE)
    5+"duck"
  })
  expect_rejects_with(as, "atomic", mp$resolve(TRUE))
  expect_true(one && two)

})

test_that("async re-error discards previous error", {
  expect_rejects_with(
    async({
      on.exit(stop("override"))
      stop("discarded")
    }),
    "override")
})

test_that("pause in on.exit handler", {

  g <- gen({
    on.exit(yield(5))
    yieldFrom(1:4)
    stop("wow")
  })
  for (i in 1:4) nextOr(g)
  expect_error(x <- nextOr(g, NULL), "wow")
  x %is% 5
  expect_error(x <- nextOr(g, NULL), "swallowed")

})

options(async.compileLevel = 0)
