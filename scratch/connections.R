test_that("read from file non-blocking", {
  # it appears that readLines is broken for
  # file connections as well?
  zzfil <- tempfile(fileext="-file")
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

  # put some more data in...
  for (i in 5:8) {
    writeLines(paste("this is line", i), f_out)
  }
  flush(f_out)

  # now, this SHOULD get line 5, but it's empty...
  readLines(f_in, 1)

  readBin(f_in, "raw") #???

})



if (capabilities("fifo")) test_that("read from non-blocking fifo", {

  # it appears that readLines is broken for fifo?
  zzfil <- tempfile(fileext="-fifo")

  f_out <- fifo(zzfil, "w+", blocking=FALSE)
  f_in <- fifo(zzfil, "r", blocking=FALSE)

  # c_in <- channel()
  # nextThen(c_in, print, print, \() cat("closing!"))

  i <- 0
  writeLines(paste("this is line", i <- i + 1), f_out)

  readLines(f_in, n=1)

  writeLines(paste("this is line", i <- i + 1), f_out)

  readChar(f_in, n=10, useBytes=TRUE)

  p <- async({
    c_in <- channel(f_in)
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


## test_that("channel from fifo", {
## })

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

## test_that("read non-blocking from socket", {

## })
