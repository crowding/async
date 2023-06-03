fn <- function(x) x

if (grepl("sendNow", getSrcFilename(fn))) {
  options(async.sendLater = FALSE)
} else {
  options(async.sendLater = TRUE)
}

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
  p1 <- nextOr(mc$ch, NULL)
  wait_for_it()
  wakeups %is% 1
  p2 <- nextOr(mc$ch, NULL)
  wait_for_it()
  wakeups %is% 2

  expect_resolves_with(p1, "a", mc$emit("a"))
  wakeups %is% 3
  expect_resolves_with(p2, "b", mc$emit("b"))
  wakeups %is% 3 # no more listeners
  expect_emits(mc$ch, "c", mc$emit("c"))
  #wakeups %is% 4 #expect_emits causes a wakeup before "emit" is called?

  mc$emit("A")
  mc$emit("B")
  mc$emit("C")

  expect_emits(mc$ch, "A")
  expect_emits(mc$ch, "B")
  expect_emits(mc$ch, "C")

  #wakeups %is% 4
  mc$reject("sdfghj")
  expect_channel_rejects(mc$ch, "sdfghj")

  wakeups <- 0
  mc <- mock_channel(wakeup = function(x) wakeups <<- wakeups + 1)
  expect_emits(mc$ch, "c", mc$emit("c"))
  wait_for_it()
  #wakeups %is% 1
  expect_channel_finishes(mc$ch, mc$finish())
  wait_for_it()
  #wakeups %is% 2

})


test_that("summarize / format channel", {

  ch1 <- mock_channel()
  pr <- then(ch1$ch(), toupper)
  wait_for_it()
  format(ch1$ch) %is%
    "<Channel (running): 0 outgoing, 1 awaiting, 0 sent>"
  ch1$emit('baz')
  wait_for_it()
  summary(ch1$ch) %is% list(state="running", outgoing=0, awaiting=0, sent=1)

})

test_that("channel of channel is idempotent", {
  ch1 <- mock_channel()$ch
  ch2 <- channel(ch1)
  expect_identical(ch2, ch1)

  ch3 <- mock_lazy_channel()$ch
  ch4 <- channel(ch3)
  expect_identical(ch4, ch3)
})

test_that("channel function checks", {

  expect_error(channel(function(x, y) NULL), "three")
  expect_error(lazy_channel(function(x, y) NULL), "three")
  expect_error(channel(function(x, ..., y, z) NULL), "three")
  expect_error(lazy_channel(function(x, y, ..., z) NULL), "three")

})


options(async.sendLater = TRUE)
options(async.compileLevel = 0)
