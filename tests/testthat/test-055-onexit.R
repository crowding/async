
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
  nextElem(g) %is% 1
  nextElem(g) %is% 2
  nextElem(g) %is% 3
  expect_false(closed)
  nextElemOr(g, "done") %is% "done"
  expect_true(closed)

  closed <- FALSE
  g <- gen({
    on.exit(closed <<- TRUE)
    expect_false(closed)
    yield(s0me+nons3nse)
  })
  expect_error(nextElem(g), "s0me")
  expect_true(closed)

})

test_that("on.exit in async; can override", {

  p <- mock_promise()
  as <- async({
    on.exit(return("changed my mind!"))
    await(p)
  })
  expect_resolves_with(as, "changed my mind!", p$reject("sad"))

  # this doesn't work in compiled.
  p <- mock_promise()
  as <- async({
    on.exit(stop("changed my mind."))
    await(p)
  }, compileLevel=-1)

  expect_rejects_with(as, "changed my mind.", p$resolve("happy"))

  # this works but the verbose traceback has me doing runExits twice...
  p <- mock_promise()
  #asyncOpts(verbose=TRUE)
  debugAsync(as, internal=FALSE)
  as <- async({
    on.exit({
      return("actually happy!")
    })
    await(p) + (s0me+n0nsense)
  })
  expect_resolves_with(as, "actually happy!", p$resolve("sad"))

})

test_that("extremely bananas: simultaneously errors and returns", {

  # okay now, here is what R does:
  f <- function(x) {on.exit(return("5!")); stop("asplode")}
  g <- NULL
  expect_error(g <- f(), "splode")
  # and yet somehow,
  g %is% "5!"
  # you can even override a stop with a return
  r <- function() {
    on.exit(return("5!"))
    stop("this throws")
  }
  g <- NULL
  expect_error(g <- r(), "throws")
  # And yet,
  g %is% "5!"

  # Can "run" replicate this?
  # FIXME: if you turn on the verbose it looks like it tries to exit twice?
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
  nextElemOr(g, NULL) %is% 1
  nextElemOr(g, NULL) %is% 2
  nextElemOr(g, NULL) %is% 3
  x <- NULL
  expect_error(x <- nextElemOr(g, NULL), "stopping")
  # an error made its way to the top, and yet,
  x %is% "done!"
  # this kinda wrecks my mental model of R error handling tbh.
  # the fact that it _also_ works if a generator is bananas.

  #??? expect_error doesn't catch this for some reason.
  #Maybe because it throws two errors?
  #expect_error(x <- nextElemOr(g, NULL), "swallowed")

  e <- NULL
  y <- tryCatch(nextElemOr(g, NULL), error=function(err) e <<- err)
  expect_match(conditionMessage(e), "swallowed")

})
