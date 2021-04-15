`%is%` <- expect_equal

  test_that("simple try", {
    g <- gen({
      x <- 5
      try({x <- 6; NULL(); x <- 7}, silent=TRUE)
      yield(x)
      x <- 8
      NULL()
      yield(x)
    })
    nextElem(g) %is% 6
    expect_error(nextElem(g), "non-function")
  })

  test_that("yield inside of try", {
    g <- gen({
      try({
        yield(5)
        stop("foo")
        yield(6)
      }, silent=TRUE)
      yield(7)
      stop("bar")
      yield(8)
    })
    expect_equal(nextElem(g), 5)
    expect_equal(nextElem(g), 7)
    expect_error(nextElem(g), "bar")
    expect_error(nextElem(g), "StopIteration")
  })

  test_that("nested tries", {

    g <- gen({
      try({
        yield(5)
        try({
          yield(55)
          stop("baz")
          yield(56)
        }, silent=TRUE)
        stop("foo")
        yield(6)
      }, silent=TRUE)
      yield(7)
      stop("bar")
      yield(8)
    })

    expect_equal(nextElem(g), 5)
    expect_equal(nextElem(g), 55)
    expect_equal(nextElem(g), 7)
    expect_error(nextElem(g), "bar")
    expect_error(nextElem(g), "StopIteration")
  })

test_that("return stops without throwing error", {
  g <- gen(
    for (i in 1:10) {
      if (i == 2) return()
      yield(i)
    })
  nextElem(g) %is% 1
  expect_error(nextElem(g), "StopIteration")
})

test_that("Catch internal errors", {
  # tryCatch should also catch errors arising from within interpreted functions.
  # For instance FALSE || NULL will throw an error from ||_cps, because
  # it just uses || internally and that throws an error.
  g <- gen({
    try(yield(yield(FALSE) || yield(NULL)), silent=TRUE)
    yield("done")
  })
  nextElem(g) %is% FALSE
  nextElem(g) %is% NULL
  nextElem(g) %is% "done"
})

test_that("try/finally, stop and return", {

  g <- gen(tryCatch({yield("Hello"); return(); yield("Goodbye")},
                    finally={yield("Hola"); stop("oops"); yield("Adios")}))
  nextElem(g) %is% "Hello"
  nextElem(g) %is% "Hola"
  expect_error(nextElem(g), "oops")

  g <- gen(tryCatch({yield("Hello"); stop("oops"); yield("Goodbye")},
                    finally={yield("Hola"); return(); yield("Adios")}))
  nextElem(g) %is% "Hello"
  nextElem(g) %is% "Hola"
  expect_error(nextElem(g), "oops")

  g <- gen(tryCatch({yield("Hello"); return(); yield("Goodbye")},
                    finally={yield("Hola"); return(); yield("Adios")}))
  nextElem(g) %is% "Hello"
  nextElem(g) %is% "Hola"
  expect_error(nextElem(g), "StopIteration")
})

if(FALSE) {

  # hmm... to do an on.exit I have to dynamically modify the call
  # graph??? or at least there needs to be some special handling at
  # the end to gather up the list of exit handlers. Collection of exit handlers maybe... I convinced myself
  # that tryCatch is simpler to start with.
  test_that("on.exit normal", {
    exited <- FALSE
    g <- gen({
      on.exit(exited <<- TRUE)
      yield(1);
    })

    nextElem(g) %is% 1
    exited %is% FALSE
    expect_error(nextElem(g), "StopIteration")
    exited %is% TRUE

    exited <- FALSE
    g <- gen({
      on.exit(exited <<- TRUE)
      yield(1);
      return(1);
      yield(2);
    })
  })



  # R doesn't even warn!
  # > (function() tryCatch(return(5), finally=return(6)))()
  # [1] 6
  # > (function() {on.exit(return(5)); return(6)})()
  # [1] 5
  # > tryCatch((function() {on.exit(return(5)); stop("!")})(), error=function(e) {caught <<- TRUE; cat("caught\n"); 6}) 
  # [1] 5

  test_that("simple try-catch with yield in body", {
    g <- gen(tryCatch(yield(5)))
    nextElem(g) %is% 5

    g <- gen(tryCatch({stop("already"); yield(5)}))
    tryCatch(nextElem(g),
             error=function (e) {
               e$message %is% "already"
               1234
             }) %is% 1234
  })

  test_that("try-finally", {
    g <- gen({
      tryCatch({
        yield(1); stop("someError")
      }, finally={
        yield(2)
      })
      yield (3)
    })

    nextElem(g) %is% 1
    nextElem(g) %is% 2
    tryCatch(nextElem(g) || stop("oops"),
             error = function (e) e$message %is% "someError")
  })

  test_that("try-catch with error", {
    caught <- FALSE
    g <- gen({
      tryCatch({
        yield(1)
        yield(2)
        stop("here")
        yield(33)
      }, error = function(e) {
        e$message %is% "here"
        caught <<- TRUE
      })
      yield(3)
    })

    nextElem (g) %is% 1
    nextElem (g) %is% 2
    nextElem (g) %is% 3
    caught %is% TRUE
  })

  test_that("tryCatch treats warning same as error", {
    caught <- FALSE
    g <- gen({
      tryCatch({
        yield(1)
        yield(2)
        warning("here")
        yield(33)
      }, warning = function(e) {
        e$message %is% "here"
        caught <<- TRUE
      })
      yield(3)
    })

    nextElem (g) %is% 1
    nextElem (g) %is% 2
    nextElem (g) %is% 3
    caught %is% TRUE
  })
}
