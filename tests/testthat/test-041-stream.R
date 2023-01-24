test_that("deque", {

  x <- deque()
  x$getFirst(NA) %is% NA
  x$append("a")
  x$getFirst(NA) %is% "a"
  x$getFirst(NA) %is% NA
  x$length() %is% 0
  x$append("a")
  x$append("b")
  x$prepend("c")
  x$prepend("d")
  x$prepend("e")
  x$length() %is% 5
  x$getLast(NA) %is% "b"
  x$getLast(NA) %is% "a"
  x$getLast(NA) %is% "c"
  x$getLast(NA) %is% "d"
  x$length() %is% 1
  x$append("f")
  x$append("g")
  x$append("h")
  x$getFirst(NA) %is% "e"
  x$getFirst(NA) %is% "f"
  x$getFirst(NA) %is% "g"
  x$getFirst(NA) %is% "h"

  x <- deque(len=4)
  x$append("a")
  x$prepend("z")
  x$append("b")
  x$length() %is% 3
  x$prepend("y")
  x$length() %is% 4
  x$append("c")
  x$length() %is% 5
  x$getFirst() %is% "y"
  x$getFirst() %is% "z"
  x$getFirst() %is% "a"
  x$getFirst() %is% "b"
  x$getFirst() %is% "c"
  x$getFirst(NULL) %is% NULL

  x <- deque(len=4)
  for (i in 1:100) {
    for (j in 1:i)
      x$append(j)
    for (j in 1:i)
      assert(x$getFirst(NULL) == j)
    assert(is.null(x$getFirst(NULL)))
    #
    for (j in i:1)
      x$prepend(j)
    for (j in i:1)
      assert(x$getLast(NULL) == j)
    assert(is.null(x$getFirst(NULL)))
    #
    for (j in 1:i)
      x$append(j)
    for (j in i:1)
      assert(x$getLast(NULL) == j)
    assert(is.null(x$getLast(NULL)))
    #
    for (j in i:1)
      x$prepend(j)
    for (j in i:1)
      assert(x$getLast(NULL) == j)
    assert(is.null(x$getLast(NULL)))
  }
  x$length() %is% 0

})

test_that("channel", {

  wakeups <- 0
  mc <- mock_channel(wakeup = function(x) wakeups <<- wakeups + 1)
  p1 <- nextElem(mc)
  wakeups %is% 1
  p2 <- nextElem(mc)
  wakeups %is% 2

  expect_resolves_with(p1, "a", mc$emit("a"))
  wakeups %is% 3
  expect_resolves_with(p2, "b", mc$emit("b"))
  wakeups %is% 3 # no more listeners
  expect_emits(mc, "c", mc$emit("c"))
  wakeups %is% 4 #expect_emits causes a wakeup before "emit" is called

  mc$emit("A")
  mc$emit("B")
  mc$emit("C")

  expect_emits(mc, "A")
  expect_emits(mc, "B")
  expect_emits(mc, "C")

  wakeups %is% 4
  mc$reject("sdfghj")
  expect_channel_rejects(mc, "sdfghj")

  wakeups <- 0
  mc <- mock_channel(wakeup = function(x) wakeups <<- wakeups + 1)
  expect_emits(mc, "c", mc$emit("c"))
  wakeups %is% 1
  expect_finishes(mc, mc$close())
  wakeups %is% 2

})


test_that("awaitNext from async()", {

  ch <- mock_channel()
  as <- async({
    l <- 0
    for (i in 1:5) {
      l <- awaitNext(ch) + l
    }
    l
  })
  ch$emit(2)
  ch$emit(4)
  ch$emit(6)
  ch$emit(8)
  wait_for_it()
  expect_resolves_with(as, 30, ch$emit(10))

})

test_that("while/break loop over channel", {

  ch <- mock_channel()
  as <- async({
    l <- 0
    repeat {
      l <- awaitNext(ch, break) + l
    }
    l
  })
  ch$emit(3)
  ch$emit(6)
  ch$emit(9)
  expect_resolves_with(as, 18, ch$close())

})

test_that("async with for loop over channel", {

  ch <- mock_channel()
  as <- async({
    l <- 0
    for (i in ch) l <- l + i
    l
  })
  debugAsync(as, internal=FALSE)
  ch$emit(100)
  ch$emit(10)
  ch$emit(1)
  expect_resolves_with(as, 111, ch$close())

})

test_that("async for loop handles channel error", {

  ch <- mock_channel()
  as <- async({
    tryCatch({
      l <- 0
      for (i in ch) l <- l + i
      l
    }, error=function(x) c(conditionMessage(x), l))
  })
  ch$emit(10)
  expect_resolves_with(as, c("ASDFGHJK", "10"), ch$reject("ASDFGHJK"))

})

test_that("async awaitNext handles channel error", {

  ch <- mock_channel()
  as <- async({
    on.exit(return(c("finally", l)))
    l <- 0
    repeat(l <- awaitNext(ch, break) + l)
    l
  })
  ch$emit(10)
  expect_resolves_with(as, c("finally", "10"), ch$reject("ASDFGHJK"))

})