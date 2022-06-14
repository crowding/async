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

  g <- gen({
    tryCatch({
      yield(5)
      stop("foo")
      yield(6)
    }, error=identity)
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

test_that("simple try-catch with yield in body", {
  g <- gen(tryCatch(yield(5)))
  nextElem(g) %is% 5

  g <- gen(tryCatch({stop("already"); yield(5)}))
  expect_error(nextElem(g), "already")
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
  expect_error(nextElem(g), "someError")
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

test_that("try-catch yield in error", {

  g <- gen({
    yield(
      tryCatch({
        yield("one")
        stop("foo")
      }, error={
        yield("two")
        function(e) {"three"}
      })
    )
  })

  nextElem(g) %is% "one"
  nextElem(g) %is% "two"
  nextElem(g) %is% "three"
  expect_error(nextElem(g), "StopIteration")
})

test_that("try-catch error in catch", {

  g <- gen({
    tryCatch({
      yield("one")
      stop("first")
    }, error=function(e) stop("second"))
  })
  nextElem(g) %is% "one"
  expect_error(nextElem(g), "second")
  expect_match(format(g), all=FALSE, "second")

  g <- gen({
    tryCatch({
      yield("one")
      stop("first")
    }, error={
      yield("two")
      stop("second")
    })
  })
  nextElem(g) %is% "one"
  nextElem(g) %is% "two"
  expect_error(nextElem(g), "second")
  expect_match(format(g), all=FALSE, "second")


  g <- gen({
    yield({
      tryCatch({
        tryCatch({
          yield("one")
          stop("first")
        }, error={
          yield("two")
          stop("second")
        })
      }, error={
        yield("three")
        function(e) conditionMessage(e)
      })
    })
  })

  nextElem(g) %is% "one"
  nextElem(g) %is% "two"
  nextElem(g) %is% "three"
  nextElem(g) %is% "second"
  expect_error(nextElem(g), "StopIteration")
})


test_that("Catch internal errors", {
  # tryCatch should also catch errors arising from within
  # interpreted functioncs.  For instance FALSE || NULL will throw an
  # error from ||_cps, because it just uses || internally and that
  # throws an error.
  g <- gen({
    tryCatch(yield(yield(FALSE) || yield(NULL)),
             error=yield("done"))
    yield("tada!")
  })
  nextElem(g) %is% FALSE
  nextElem(g) %is% NULL
  nextElem(g) %is% "done"
  nextElem(g) %is% "tada!"
})

test_that("try-catch-finally", {
  
  g <- gen({
    yield(
      tryCatch({
        yield("one")
        stop()
      }, error={
        yield("two")
      }, finally={
        yield("three")
      })
    )
  })

  nextElem(g) %is% "one"
  nextElem(g) %is% "two"
  nextElem(g) %is% "three"
  nextElem(g) %is% "two"
  expect_error(nextElem(g), "StopIteration")

  g <- gen({
    tryCatch({
      yield("one")
      stop()
    }, error={
      yield("two")
      stop("oops")
    }, finally={
      yield("three")
    })
  })

  nextElem(g) %is% "one"
  nextElem(g) %is% "two"
  nextElem(g) %is% "three"
  expect_error(nextElem(g), "oops")
})

test_that("try-catch-finally and break, next, return", {
  g <- gen({
    repeat {
      tryCatch({
        yield("one")
        stop()
      }, error={
        yield("two")
        break
      }, finally={
        yield("three")
      })
    }
    yield("four")
  })
  nextElem(g) %is% "one"
  nextElem(g) %is% "two"
  nextElem(g) %is% "three"
  nextElem(g) %is% "four"
  expect_error(nextElem(g), "StopIteration")

  g <- gen({
    repeat {
      tryCatch({
        yield("one")
        next
      }, finally={
        yield("two")
        break
      })
    }
    yield("three")
  })

  nextElem(g) %is% "one"
  nextElem(g) %is% "two"
  nextElem(g) %is% "three"
  expect_error(nextElem(g), "StopIteration")

  g <- gen({
    repeat {
      tryCatch({
        yield("one")
        break
      }, finally={
        yield("two")
        next
      })
    }
    yield("three")
  })

  nextElem(g) %is% "one"
  nextElem(g) %is% "two"
  nextElem(g) %is% "one"
  nextElem(g) %is% "two"

  g <- gen({
    repeat {
      tryCatch({
        yield("one")
        return()
      }, finally={
        yield("two")
      })
    }
    yield("three")
  })

  nextElem(g) %is% "one"
  nextElem(g) %is% "two"
  expect_error(nextElem(g), "StopIteration")
  expect_output(print(g), "finished")
})

test_that("Nested try-catch-finally", {

  g <- gen({
    tryCatch({
      yield("body 1")
      tryCatch({
        yield("body 2")
        stop("foo")
      }, error = {
        yield("error 2")
        function(e) stop(e)
      }, finally = {
        yield("finally 2")
      })
    }, error = {
      yield("error 1")
    }, finally = {
      yield("finally 1")
    })
  })

  nextElem(g) %is% "body 1"
  nextElem(g) %is% "body 2"
  nextElem(g) %is% "error 2"
  nextElem(g) %is% "finally 2" # this how R does it...
  nextElem(g) %is% "error 1"
  nextElem(g) %is% "finally 1"

  # though there's an argument for asyncs to "fail fast," i.e.
  # error2 -> error1 -> REJECT -> finally2 -> finally1
})

test_that("break/next/return in trycatch", {
  g <- gen({
    i <- 1
    repeat {
      repeat {
        tryCatch({
          if (razz <- (i %% 2 == 0)) yield("Razz")
          if (fizz <- (i %% 3 == 0)) yield("Fizz")
          if (buzz <- (i %% 5 == 0)) yield("Buzz")
          if (razz && buzz) stop()
          if (fizz && buzz) break
          if (razz || fizz || buzz) next
          yield(toString(i))
          if (i > 30) return()
        }, error = {
          yield("\n---")
        }, finally = {
          yield("\n")
          i <- i + 1
        })
      }
      yield("<>\n")
    }
  })
  (unlist(strsplit(paste0(as.list(g), collapse = ""), "\n"))) %is%
    c("1", "Razz", "Fizz", "Razz", "Buzz",
    "RazzFizz", "7", "Razz", "Fizz", "RazzBuzz", "---",
    "11", "RazzFizz", "13", "Razz", "FizzBuzz", "<>",
    "Razz", "17", "RazzFizz", "19", "RazzBuzz", "---",
    "Fizz", "Razz", "23", "RazzFizz", "Buzz",
    "Razz", "Fizz", "Razz", "29", "RazzFizzBuzz", "---",
    "31")
})

