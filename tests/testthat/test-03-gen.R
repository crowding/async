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

test_that("yieldFrom", {

  a <- list("foo", "bar", "baz")
  b <- iseq(1, 3)
  gchain <- function(its) {
    itors <- iteror(its)
    gen(for (it in itors) yieldFrom(it))
  }

  gchain2 <- function(its) { force(its)
    gen(for (it in its) for (i in it) yield(i))
  }

  as.list(gchain(list(a, b))) %is% list("foo", "bar", "baz", 1, 2, 3)
  b <- iseq(1, 3)
  as.list(ichain(list(a, b))) %is% list("foo", "bar", "baz", 1, 2, 3)

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

test_that("generator with split pipes", {

  x <- iseq(1, 55)
  incomplete <- gen(split_pipes=TRUE, {
    repeat {
      sum <- 0
      for (i in 1:10) {
        sum <- nextElemOr(x, {yield(sum); return()}) + sum
      }
      yield(sum)
    }
  })

  n <- as.numeric(as.list(incomplete))
  n[6] %is% sum(51:55)
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

  # FIXME: this test doesn't work, sys.nframe() is getting "0" somehow?
  expect_true(s2 >= s1)
})

test_that("tracing", {
  g <- gen({{j <- 0; i <- 0}; for (i in 1:10) yield(j <- j + i)},
           trace=with_prefix("triangle"))
  expect_output(
    nextElem(g),
    "triangle: R: + i <- 0.*triangle: R: j <- j \\+ i.*triangle: generator: yield.*")
})

test_that("run", {

  total_ <- 0
  output_ <- collect( function(yield) {
    for (i in seq(2, 48, by=7))
      total_ <<- yield(total_+i)
  })

  total <- 0
  output <- run(
    for (i in iseq(2, 48, by=7))
      total <- yield(total+i))

  total %is% total_
  output %is% output_

  expect_error(
    for (i in iseq(2, 2400, by=7))
      total <- yield(total+i))

  run(if(TRUE) yield(1) else 5) %is% list(1)
  run(if(FALSE) yield(1) else 5) %is% list()
  run(if(FALSE) return(0) else 5) %is% 5

  expect_error(
    run(for (i in iseq(1, 1000, by=23))
      if (i%%37 == 0) stop("oops") else yield(i)),
    "oops")

  expect_error(
    run(for (i in iseq(1, 1000, by=23))
      if (i%%37 == 0) yield(sOmE+nOnSeNsE) else yield(i)),
    "not found")

  a <- list(1:2, 1:3, 1:4)
  run(for (i in a) yieldFrom(i), 0) %is% c(1:2, 1:3, 1:4)
  expect_error(run(yieldFrom(a), 0), "replace")

  b1 <- run(for (i in 1:10) for(j in iseq(1, i)) yield(j), 0)
  expect_length(b1, sum(1:10))
  b2 <- run(for (i in 1:10) yieldFrom(iseq(1, i)), 0)
  b1 %is% b2

})

# moved here from test_cps where it felt too early.
test_that("generator works wnen async package not attached", {

  if ("package:async" %in% search()) {
    on.exit({
      attachNamespace("async")
    }, add=TRUE)
    detach("package:async")
  }
  g <- async::gen({
    max <- 10
    skip <- 4
    i <- 0;
    repeat {
      i <- i + 1;
      if (i %% skip == 0) next
      if (i > max) break
      yield(i)
    }
  }, globalenv())

  # can run a generator without having the package attached
  # this should really be in the next test though
  l <- as.numeric(as.list((g)))
  l %is% c(1, 2, 3, 5, 6, 7, 9, 10)
})
