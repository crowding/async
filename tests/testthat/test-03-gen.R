#' @import nseval

`%is%` <- expect_equal

test_that("generators", {
  x <- gen({yield("one"); yield("two")})
  as.list(x) %is% list("one", "two")

  x <- gen({yield("one"); yield("two"); print("threeeee")})
  expect_output(as.list(x) %is% list("one", "two"), "threee")
})

test_that("generator loop", {
  ii <- function(n) gen(for (i in 1:n) yield(i))
  i <- ii(10)
  for (j in 1:10) {
    nextElem(i) %is% j
  }
  expect_error(nextElem(i), "StopIteration")

  i <- ihasNext(ii(10))
  for (j in 1:10) {
    hasNext(i) %is% TRUE
    nextElem(i) %is% j
  }
  hasNext(i) %is% FALSE
  expect_error(nextElem(i), "StopIteration")

})

test_that("further nextElems will error with stopIteration", {

  g <- gen(yield(1))
  nextElem(g) %is% 1
  expect_error(nextElem(g), "StopIteration")
  expect_error(nextElem(g), "StopIteration")
  g <- gen({
    yield(1)
    stop("foo")
  })
  nextElem(g) %is% 1
  expect_error(nextElem(g), "foo")
  expect_error(nextElem(g), "StopIteration")

})

test_that("a generator", {
  x <- gen(for (i in 1:10) yield(i))
  as.numeric(as.list(x)) %is% 1:10
})

test_that("for loop over an iterator", {
  x <- gen(for (i in icount()) {yield(i)})

  as.numeric(as.list(ilimit(x, 10))) %is% 1:10

  j <- gen(for(i in 1:10) if (i %% 7 == 0) stop("oops") else yield(i))
  x <- 0
  g <- gen(for(i in j) if (FALSE) yield(NULL) else x <<- x + 1)
  expect_error(nextElem(g), "oops")
})

test_that("nested for loops", {
  x <- gen({
    for (i in iterators::iter(c(3,2,1,2), recycle=TRUE)) {
      for (j in 1:i)
        yield(1)
      yield(0)
    }
  })
  as.numeric(as.list(ilimit(x, 24))) %is%
    c(1,1,1,0,1,1,0,1,0,1,1,0,1,1,1,0,1,1,0,1,0,1,1,0)
})

test_that("generators create local scope", {
  x <- 4
  g <- gen({
    while (x <= 10) {
      x <- x + 1
      yield(x)
    }
  })
  as.numeric(as.list(g)) %is% 5:11
  x %is% 4
})

test_that("generators reject recursion", {
  g <- gen(yield(nextElem(g)))
  expect_error(nextElem(g), "running")

  f <- gen(repeat yield(nextElem(g)))
  g <- gen(repeat yield(nextElem(f)))
  expect_error(nextElem(g), "running")
})

test_that("generator format", {
  g <- gen({x <- 0; while(x <= 12) x <- yield(x + 5)})

  expect_output(print(g), "yielded")
  expect_output(print(g), "while \\(x <= 12\\) x <- yield\\(x \\+ 5\\)")
  as.list(g)
  expect_output(print(g), "finished")
  g <- gen({x <- 0; repeat {if (x > 12) stop("oops"); x <- yield(x + 5)}})
  expect_error(as.list(g), "oops")
  expect_output(print(g), "(stopped:.*oops|finished)")

  g <- gen(yield(utils::capture.output(print(g))))
  expect_output(cat(nextElem(g)), "running")
})

test_that("last statement is forced", {
  hello <- NULL
  g <- gen({yield("one"); yield("two"); hello <<- "three"})
  nextElem(g) %is% "one"
  nextElem(g) %is% "two"
  expect_error(nextElem(g), "StopIteration")
  expect_equal(hello, "three")
})

test_that("can optionally split pipes", {
  expect_error(gen(repeat x <- yield(x)[x]), "split_pipes")
  x <- c(2, 4, 1, 3)
  g <- gen(repeat x <- yield(x)[x], split_pipes=TRUE)
  nextElem(g) %is% c(2, 4, 1, 3)
  nextElem(g) %is% c(4, 3, 2, 1)
  nextElem(g) %is% c(1, 2, 3, 4)
})

test_that("Dummy", {
  expect_error(yield(5), "outside")
  expect_error( gen(await(yield(5))), "await" )
})

test_that("tailcalls", {
  x <- gen({for (i in 1:10) if(FALSE) yield("no"); yield({sys.nframe()})},
           eliminate.tailcalls=TRUE)
  s1 <- nextElem(x)
  x <- gen({for (i in 1:10) if(FALSE) yield("no"); yield(sys.nframe())},
           eliminate.tailcalls=FALSE)
  s2 <- nextElem(x)

  # FIXME: this test doesn't work, sys.nframe() is getting "0"?
  expect_true(s2 >= s1)
})

test_that("tracing", {
  g <- gen({{j <- 0; i <- 0}; for (i in 1:10) yield(j <- j + i)},
           trace=with_prefix("triangle"))
  expect_output(
    nextElem(g),
    "triangle: R: + i <- 0.*triangle: R: j <- j \\+ i.*triangle: generator: yield.*")
})
