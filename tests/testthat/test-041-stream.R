fn <- function() {
  x
}
setCompileLevelFromFn(fn)

if (grepl("sendNow", getSrcFilename(fn))) {
  options(async.sendLater = FALSE)
} else {
  options(async.sendLater = TRUE)
}

test_that("awaitNext from async()", {

  mch <- mock_channel()
  as <- async({
    l <- 0
    for (i in 1:5) {
      l <- awaitNext(mch$ch) + l
    }
    l
  })

  mch$emit(2)
  mch$emit(4)
  mch$emit(6)
  mch$emit(8)
  wait_for_it()
  expect_resolves_with(as, 30, mch$emit(10))

})

test_that("while/break loop over channel", {

  ch <- mock_channel()
  as <- async({
    l <- 0
    repeat {
      l <- awaitNext(ch$ch, break) + l
    }
    l
  })
  ch$emit(3)
  ch$emit(6)
  ch$emit(9)
  # If closed happen in the event loop, make sure that
  # listeners can get the rest...
  expect_resolves_with(as, 18, ch$finish())

})

test_that("async with for loop over channel", {

  ch <- mock_channel()
  as <- async({
    l <- 0
    for (i in ch$ch) l <- l + i
    l
  })
  debugAsync(as, internal=FALSE)
  ch$emit(100)
  ch$emit(10)
  ch$emit(1)
  expect_resolves_with(as, 111, ch$finish())

})

test_that("async for loop handles channel error", {

  ch <- mock_channel()
  as <- async({
    tryCatch({
      l <- 0
      for (i in ch$ch) l <- l + i
      l
    }, error=function(x) c(conditionMessage(x), l))
  })
  ch$emit(10)
  expect_resolves_with(as, c("ASDFGHJK", "10"), ch$reject("ASDFGHJK"))

})

test_that("async awaitNext with or", {

  as <- async(function(ch) {
    on.exit(return(c("finally", l)))
    l <- 0
    repeat(l <- awaitNext(ch, break) + l)
    l
  })
  as1 <- as((mch <- mock_channel())$ch)
  mch$emit(10)
  mch$emit(20)
  expect_resolves_with(as1, c("finally", "30"), mch$reject("ASDFGHJK"))

})

test_that("async awaitNext with handler", {

  as <- async(function(ch){
    l <- 10
    stopping <- FALSE
    while (!stopping) {
      l <- awaitNext(ch,
                     or=break,
                     error=function(err) {
                       stopping <<- TRUE
                       conditionMessage(err)
                     })
    }
    l
  })

  as1 <- as((mch <- mock_channel())$ch)
  mch$emit(10)
  expect_resolves_with(as1, "ASDFGHJK", mch$reject("ASDFGHJK"))

  as2 <- as((mch <- mock_channel())$ch)
  mch$emit(10)
  expect_resolves_with(as2, 10, mch$finish())

})

test_that("await a finished channel just calls finish()", {

  mch <- mock_channel()
  as <- async({
    for (i in 1:10) (awaitNext(mch$ch, next))
    i
  })
  expect_resolves_with(as, 10, mch$finish())

})

test_that("stream: can await and yield", {

  p1 <- mock_promise()
  p2 <- mock_promise()
  st <- stream({
    x <- await(p1)
    yield(x)
    x <- await(p2) + x
    yield(x)
  })

  p3 <- nextOr(st, NULL)
  #debugAsync(st)
  p4 <- nextOr(st, NULL)
  p1$resolve(10)
  expect_resolves_with(p3, 10, NULL)
  expect_resolves_with(p4, 25, p2$resolve(15))
  expect_channel_finishes(st, NULL)

})

test_that("lazy vs eager streams", {

  ch2 <- mock_channel()
  ct2 <- 0
  running <- FALSE
  lazy <- stream(lazy=TRUE, {
    running <<- TRUE
    on.exit(running <<- FALSE)
    for (i in ch2$ch) {
      yield(2*i)
      ct2 <<- ct2 + 1
    }
  })
  running %is% FALSE
  pr <- nextOr(lazy, NULL)
  wait_for_it()
  running %is% TRUE
  expect_resolves_with(pr, 24, ch2$emit(12))
  ct2 %is% 0
  pr <- nextOr(lazy, NULL)
  wait_for_it()
  ct2 %is% 1
  ch2$emit(18)
  wait_for_it()
  ct2 %is% 1
  expect_resolves_with(pr, 36, NULL)
  ct2 %is% 1
  expect_emits(lazy, 10, ch2$emit(5))
  ct2 %is% 2
  ch2$finish()
  running %is% TRUE
  expect_channel_finishes(lazy)
  running %is% FALSE

  ch1 <- mock_channel()
  ct1 <- 0
  running <- FALSE
  eager <- stream(lazy=FALSE, {
    running <<- TRUE
    on.exit(running <<- FALSE)
    for (i in ch1$ch) {
      yield(2*i)
      ct1 <<- ct1 + 1
    }
  })

  running %is% TRUE
  pr <- nextOr(eager, NULL)
  wait_for_it()
  expect_resolves_with(pr, 24, ch1$emit(12))
  ct1 %is% 1
  pr <- nextOr(eager, NULL)
  wait_for_it()
  ct1 %is% 1
  ch1$emit(18)
  wait_for_it()
  ct1 %is% 2
  expect_resolves_with(pr, 36)
  ct1 %is% 2
  expect_emits(eager, 10, ch1$emit(5))
  ct1 %is% 3
  ch1$finish()
  wait_for_it()
  running %is% FALSE
  expect_channel_finishes(eager)

})

test_that("collecting stream", {
  st <- stream(for (i in 1:10) yield(i))
  expect_resolves_with(gather(st), as.list(1:10))
})

test_that("stream function", {

  f <- stream(function(s, add=0) {
    for (elem in s) {
      yield(elem+add)
    }
  })

  a <- mock_channel()
  b <- mock_channel()
  fa <- f(a$ch, 5)
  fb <- f(b$ch, 6)
  ga <- gather(fa, 0) # why is this getting here
  gb <- gather(fb, 0)

  a$emit(1)
  b$emit(2)
  a$emit(5)
  b$emit(10)
  expect_resolves_with(ga, c(6, 10), a$finish())
  expect_resolves_with(gb, c(8, 16), b$finish())

})

test_that("stream stop", {

  s <- stream(tryCatch({
    yield(1)
    yield(5+"monkey")
  },
  finally=yield(5)))

  expect_emits(s, 1, NULL)
  expect_emits(s, 5, NULL)
  expect_channel_rejects(s, "non-numeric")

})


test_that("yieldFrom channel", {

  it <- iteror(1:10)
  ch <- stream(for (i in 11:20) yield(i))
  st <- stream({
    yieldFrom(it)
    yieldFrom(ch)
  })

  expect_resolves_with(gather(st, 0), 1:20)

  st1 <- stream(for (i in 1:10) if (i %% 7 == 0) stop("yyy") else yield(i))
  st2 <- stream(yieldFrom(st1))
  pr <- gather(st2, 0)
  then(pr,
       \(val) stop("should have rejected"),
       \(val) expect_equal(attr(val, "partialResults"), 1:6))
  wait_for_it()

})

test_that("stream format", {

  pr <- mock_promise()
  st <- stream({yield(await(pr))})
  expect_output(print(st), "stream\\(\\{")

  expect_output(print(st), "\\[yielded")
  expect_output(print(st), "<environment: ")
  expect_output(print(st), "<Channel")
  g <- gather(st)
  wait_for_it()
  expect_output(print(st), "1 awaiting, 0 sent")
  pr$resolve(5)
  wait_for_it()
  expect_output(print(st), "resolved at `")
  expect_output(print(g), "fulfilled: list")
  expect_output(print(st), "0 awaiting, 1 sent")

  pr <- mock_promise()
  st <- stream({yield(await(pr))})
  g <- gather(st)
  capture.output({pr$reject("oops"); wait_for_it()},
                 type="message")
  expect_output(print(st), "rejected at `")
  expect_output(print(st), "simpleError: oops")
  expect_output(print(g), "rejected: simpleError")
  expect_output(print(st), "0 awaiting, 1 sent")

})



options(async.sendLater = TRUE)
options(async.compileLevel = 0)
