mock_promise <- function() {
  resolve <- NULL
  reject <- NULL
  value <- NULL
  err <- NULL
  state <- "pending"
  p <- promise(function(resolve, reject) {resolve <<- resolve; reject <<- reject})
  then(p, function(x) {value <<- x; state <<- "resolved"},
       function(e) {err <<- e; state <<- "rejected"} )
  p$resolve <- resolve
  p$reject <- reject
  p$state <- function() switch(state,
                               pending = list(state=state),
                               resolved = list(state=state, value=value),
                               rejected = list(state=state, err=err))
  structure(p, class=c("promise", "mock_promise"))
}

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
  expect_true(resolved)
  p <- mock_promise()
  then(p,
       onRejected = function(e) {
         rejected <<- TRUE
         expect_equal(conditionMessage(e), "foo")
       })
  p$reject("foo")
  expect_true(rejected)
})

test_that("async with no await resolves immediately", {
  p <- make_async(arg_cps(5))
  result <- NULL
  then(p, function(x) {result <<- x})
  later::run_now()
  expect_equal(result, 5)
  a <- make_async(arg_cps(stop("oops")))
  e <- NULL
  then(p, onRejected=function(err) e <<- err)
  later::run_now()
  expect_equal(conditionMessage(e), "oops")
})

test_that("async with one await", {
  pr <- mock_promise()
  a <- 0
  as <- make_async(`{_cps`(
    `<-_cps`(arg_cps(a), await_cps(arg_cps(pr))),
    arg_cps(a + 5)))
  result <- NULL
  then(as, function(x) result <<- x)
  pr$resolve(10)
  later::run_now()
  expect_equal(result, 15)
})

test_that("more than one await", {
  p1 <- mock_promise()
  p2 <- mock_promise()
  asy <- make_async(`{_cps`(
    `&&_cps`(await_cps(arg_cps(p1)),
             await_cps(arg_cps(p2)))))
  result <- NULL
  then(asy, function(x) result <<- x)
  p1$resolve(FALSE)
  later::run_now()
  expect_false(result)

  p1 <- mock_promise()
  p2 <- mock_promise()
  asy <- make_async(`{_cps`(
    `&&_cps`(await_cps(arg_cps(p1)),
             await_cps(arg_cps(p2)))))
  result <- NULL
  then(asy, function(x) result <<- x)
  p1$resolve(TRUE)
  later::run_now()
  expect_identical(result, NULL)
  p2$resolve(FALSE)
  later::run_now()
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
  later::run_now()
  expect_identical(result, NULL)
  p3$resolve(42)
  later::run_now()
  expect_equal(result, 42)
})
