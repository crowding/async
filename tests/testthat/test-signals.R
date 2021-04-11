`%is%` <- expect_equal

if(exists("experimental", envir = globalenv()) && globalenv()$experimental) {

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
    expect_equal(nextElem(g), 7)
    expect_error(nextElem(g), "bar")
    expect_error(nextElem(g), "StopIteration")
  })

}

if(FALSE) {

  # R doesn't even warn!
  # > (function() tryCatch(return(5), finally=return(6)))()
  # [1] 6
  # > (function() {on.exit(return(5)); return(6)})()
  # [1] 5
  # > tryCatch((function() {on.exit(return(5)); stop("!")})(), error=function(e) {caught <<- TRUE; cat("caught\n"); 6}) 
  # [1] 5

  
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
