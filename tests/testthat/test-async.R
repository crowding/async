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

# if(exists("experimental", envir = globalenv()) && globalenv()$experimental) {
if(FALSE) {
  test_that("test mock promise", {
    p <- mock_promise()
    resolved <- FALSE
    finally <- FALSE
    rejected <- FALSE
    then(p,
         onFulfilled = function(x) {
           resolved <<- TRUE
           expect_equal(x, 5)
         }
         )
    p$resolve(5)
    expect_true(resolved)
    p <- mock_promise()
    then(p,
         onRejected = function(e) {
           browser()
           rejected <<- TRUE
           expect_equal(conditionMessage(e), "foo")
         })
    p$reject("foo")
    expect_true(rejected)
  })

  test_that("async with no await resolves immediately", {
    p <- make_async(arg_cps(5))
    then(p, function(x) {cat("triggered!"); 5})
  })

  test_that("async with one await", {
    pr <- mock_promise
    a <- 0
    p <- make_async(`{_cps`(
      arg_cps(print("Triggered!")),
      `<-_cps`(arg_cps(a), await_cps(arg_cps(pr))),
      a + 5))
  })

}
