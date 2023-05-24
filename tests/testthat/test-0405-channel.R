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
  expect_channel_closes(mc$ch, mc$close())
  wait_for_it()
  #wakeups %is% 2

})

test_that("combining channels", {

  pr1 <- mock_promise()
  ch1 <- mock_channel()
  pr2 <- mock_promise()

  out <- combine(pr1,ch1$ch,pr2)

  expect_emits(out, "foo", pr1$resolve("foo"))
  expect_emits(out, "bar", ch1$emit("bar"))
  expect_emits(out, "baz", ch1$emit("baz"))
  expect_emits(out, "qux", pr2$resolve("qux"))
  expect_channel_closes(out, ch1$close())

})

test_that("read from file non-blocking", {
  # it appears that readLines is broken for
  # file connections as well?
  zzfil <- tempfile(fileext="-fifo")
  f_out <- file(zzfil, "w")
  f_in <- file(zzfil, "rb", blocking=FALSE)

  for (i in 1:4) {
    writeLines(paste("this is line", i), f_out)
  }
  flush(f_out)

  for (i in 1:4)
    print(readLines(f_in, n=1))
    #so far so good...

  # calling readLines again returns empty, as expected...  
  print(readLines(f_in, n=1))

  # put some mode data in...
  for (i in 5:8) {
    writeLines(paste("this is line", i), f_out)
  }
  flush(f_out)

  # now, this SHOULD get line 5, but it's empty...
  readLines(f_in, 1)

  readBin(f_in, "raw")

})

if (capabilities("fifo")) test_that("read from non-blocking fifo", {

  # it appears that readLines is broken for fifo?
  zzfil <- tempfile(fileext="-fifo")
  
    f_in <- fifo(zzfil, "r", blocking=FALSE)
    f_in <- f_out <- fifo(zzfil, "w+", blocking=FALSE)
    
    # c_in <- channel()
    # nextThen(c_in, print, print, \() cat("closing!"))

    i <- 0
    writeLines(paste("this is line", i <- i + 1), f_out)

    readLines(f_in, n=1)

    close(f_in)
    close(f_out)
    
    readChar(f_con, n=10, useBytes=TRUE)
    
    p <- async({
      first <- awaitNext(c_in)
      for (ln in c_in) {
        cat("Received:", ln, "\n")
      }
    })

    writeLines("abc", f_out)
    nextThen(cin)
    wait_for_it()
    print(readLines(zz))
    close(zz)
    unlink(zzfil)

  })

}

test_that("channel from fifo", {
})

test_that("read from pipe?", {
  # alas, pipe does not have a non-blocking option
  fnam <- tempfile()
  f_out <- file(fnam, open="w")
  f_in <- pipe(paste("tail -f", fnam), open="r")
  writeLines("a line", f_out)
  flush(f_out)
  readLines(f_in, n=1)

  f_in <- f_out <- fifo(zzfil, "w+", blocking=FALSE)

})

test_that("read non-blocking from socket", {
  
})

options(async.sendLater = TRUE)
options(async.compileLevel = 0)
