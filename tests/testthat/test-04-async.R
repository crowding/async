test_that("test mock promise", {
  p <- mock_promise()
  resolved <- FALSE
  finally <- FALSE
  rejected <- FALSE
  then(p,
       onFulfilled = function(x) {
         resolved <<- TRUE
         expect_equal(x, 5)
       })
  p$resolve(5)
  wait_for_it()
  expect_true(resolved)
  p <- mock_promise()
  then(p,
       onRejected = function(e) {
         rejected <<- TRUE
         expect_equal(conditionMessage(e), "foo")
       })
  p$reject("foo")
  wait_for_it()
  expect_true(rejected)
})

test_that("async with no await resolves immediately", {
  p <- make_async(R(5))
  result <- NULL
  then(p, function(x) {result <<- x})
  wait_for_it()
  expect_equal(result, 5)
  e <- simpleError("wat")
  then(make_async(R(stop("oops"))), onRejected=function(err) {e <<- err})
  wait_for_it()
  expect_equal(conditionMessage(e), "oops")
})

test_that("async with one await", {
  pr <- mock_promise()
  a <- 0
  as <- make_async(`{_cps`(
    `<-_cps`(R(a), await_cps(R(pr))),
    R(a + 5)))
  result <- NULL
  then(as, function(x) result <<- x)
  pr$resolve(10)
  wait_for_it()
  expect_equal(result, 15)
})

test_that("more than one await", {
  p1 <- mock_promise()
  p2 <- mock_promise()
  asy <- make_async(`{_cps`(
    `&&_cps`(await_cps(R(p1)),
             await_cps(R(p2)))))
  result <- NULL
  then(asy, function(x) result <<- x)
  p1$resolve(FALSE)
  wait_for_it()
  expect_false(result)

  p1 <- mock_promise()
  p2 <- mock_promise()
  asy <- make_async(`{_cps`(
    `&&_cps`(await_cps(R(p1)),
             await_cps(R(p2)))))
  result <- NULL
  then(asy, function(x) result <<- x)
  p1$resolve(TRUE)
  wait_for_it()
  expect_identical(result, NULL)
  p2$resolve(FALSE)
  wait_for_it()
  expect_false(result)
})

test_that("async grammar", {
  p1 <- mock_promise()
  p2 <- mock_promise()
  p3 <- mock_promise()
  asy <- async(if(await(p1)) await(p2) else await(p3))
  result <- NULL
  then(asy, function(x) result <<- x)
  p2$resolve("hello")
  p1$resolve(FALSE)
  wait_for_it()
  expect_identical(result, NULL)
  p3$resolve(42)
  wait_for_it()
  expect_equal(result, 42)
})

test_that("async format", {
  pr <- mock_promise()
  as <- suppressMessages(async({x <- await(pr); x + 5}))
  expect_output(print(as), "pending")
  expect_output(print(as), "x \\+ 5")
  pr$resolve(5)
  wait_for_it()
  expect_output(print(as), "fulfilled: numeric")

  pr <- mock_promise()
  as <- async({x <- await(pr); x + 5})
  then(pr, onFulfilled=function(val) {NULL},
       onRejected=function(err) {NULL})
  capture.output({pr$reject("oops"); wait_for_it()}, type="message")
  wait_for_it()
  expect_output(print(as), "rejected")
})

test_that("async splits pipes by default", {
  pr <- mock_promise()
  expect_error(async(await(pr) + 5, split_pipes=FALSE), "split_pipes")
  result <- NULL
  as <- async(await(pr) + 5)
  then(as,
       onFulfilled = function(x) result <<- x,
       onRejected=stop)
  pr$resolve(5)
  wait_for_it()
  result %is% 10

  pr <- mock_promise()
  x <- async({expect_equal(await(pr), 1); "done"})

  pr$resolve(1)
  wait_for_it()
  expect_output(print(x), "fulfilled: character")
})

test_that("async return", {

  notrun <- TRUE
  result <- NULL
  then(async({
    for (i in 1:10) {
      if (i > 5) await(NULL)
      if (i == 5) {
        return(i)
        notrun <<- FALSE
      }
    }
    notrun <<- FALSE
  }),
  function(val) result <<- val)
  wait_for_it()
  result %is% 5
  notrun %is% TRUE

})

test_that("async try-finally with return", {

  cleanup <- FALSE
  result <- NULL
  not_run <- TRUE
  then(
    async({
      tryCatch({
        if(FALSE) await(NULL)
        return(2)
        not_run <<- FALSE
      }, finally={
        cleanup <<- TRUE
      })
      not_run <<- FALSE
      5
    }),
    function(x) result <<- x, stop)
  wait_for_it()

  cleanup %is% TRUE
  not_run %is% TRUE
  result %is% 2
})

test_that("async try-finally with return", {
  pr <- mock_promise()
  cleanup <- FALSE
  retval <- NULL
  not_run <- TRUE
  as <- async({
    tryCatch({
      if (await(pr)) {
        return(5)
        not_run <<- FALSE
        5
      } else 4
    }, finally={
      cleanup <<- TRUE
    })
  })
  then(as, function(x) {
    retval <<- x
  })
  pr$resolve(TRUE)
  wait_for_it()
  cleanup %is% TRUE
  retval %is% 5
  not_run %is% TRUE
})

test_that("Dummy", {
  expect_error(await(5), "outside")
  expect_error(async(yield(await(5))), "yield")
})

test_that("One async awaiting another", {
  a <- mock_promise()
  b <- async(await(a) + 1)
  c <- async(await(b) + 2)
  out <- NULL
  then(c, function(x) out <<- x)
  a$resolve(5)
  wait_for_it()
  out %is% 8
})


if(FALSE) {
  # still thinking about this.  The idea is that if there is a
  # async(try({...}, finally={})), and the "try" either stops or
  # returns, then we fire the resolve/reject first, and deal with the
  # finally block later. I think this should only apply to a
  # try/finally at top level. (or on.exit)
  # In a nested clause like

  test_that("tryCatch({..., return(x)}, finally={...}) resolves promise before 'finally'",
  {
    filename <- mock_promise()
    opened <- FALSE
    closed <- TRUE
    pass <- FALSE
    dataset <- async({
      tryCatch({
        opened <<- await(filename)
        return(paste0(opened, ".Rdata"))
      }, finally={
        # it should service the next promise before coming back here
        cat("closed\n")
        closed <<- TRUE
      })
    })

    closed2 <- FALSE
    opened2 <- FALSE
    results <- async({
      tryCatch({
        await(dataset)
        opened2 <<- TRUE
        expect_false(closed)
        dataset # no "return" so finally will execute inline
      }, finally={
        cat("closed2\n")
        closed2 <<- FALSE
      })
    })

    check <- async({
      expect_equal(await(results), 5)
      expect_false(closed)
      expect_true(closed2) #
      pass <<- TRUE
    })

    wait_for_it()
    expect_true(pass)
  })
}

test_that("tracing", {
  expect_output(
    g <- async(if(TRUE) 1 else await(pr),
               trace=with_prefix("one")),
    "one: R: TRUE.*R: 1.*async: return")
})

test_that("awaiting value that doesn't exist", {

  as <- async({
    tryCatch({
      if (await(pr)) {
        return(5)
        not_run <<- FALSE
        5
      } else 4
    }, finally={
      cleanup <<- TRUE
    })
  })

  result <- NULL
  then(as, function(val) stop("should have failed!"),
       function(err) result <<- err)
  wait_for_it()
  expect_match(as.character(result), "not found")

})
