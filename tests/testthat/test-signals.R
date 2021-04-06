`%is%` <- expect_equal

if(exists("experimental", envir = globalenv()) && globalenv()$experimental) {

  test_that("try", {
    g <- gen({
      try({
        yield(5)
        stop("foo")
        yield(6)
      })
      yield(7)
    })
    nextElem(g) %is% 5
    nextElem(g) %is% 7
    expect_error(nextElem(g), "StopIteration")
  })

  test_that("on.exit", {
    exited <- FALSE
    g <- gen({
      on.exit(exited <- TRUE)
      yield(1);
      return();
    })
  })

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
