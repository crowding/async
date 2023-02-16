test_that("Switch numeric", {

  f <- function(x) {
    g <- gen({
      y <- switch(x,
                  yield("one"),
                  yield("two"),
                  yield("three"))
      yield(strrev(y))
    })
#    debugAsync(g, internal=TRUE)
    paste0(nextElemOr(g, NA), "!", nextElemOr(g, NA))
  }

  f(1) %is% "one!eno"
  f(2.01) %is% "two!owt"
  f(3.99999) %is% "three!eerht"
  # Contra R's behavior, you must choose an option.
  expect_error(f(0), "bounds")
  expect_error(f(-1), "bounds")
  expect_error(f(0.99999), "bounds")
  expect_error(f(4), "bounds")
  expect_error(f("character"), "numeric")

})

test_that("Switch string", {

  g <- function(x) {
    g <- gen({
      y <- switch(x,
                  one=yield(1),
                  two=yield(2))
      yield(y + nchar(x))
    })
    c(nextElemOr(g, NULL), nextElemOr(g, NULL))
  }

  g("one") %is% c(1, 4)
  g("two") %is% c(2, 5)
  # again contra R's behavior.
  expect_error(g("ONE"), "default")
  expect_error(g(1), "character")

})

test_that("Switch string with default", {

  h <- function(x) {
    g <- gen({
      y <- switch(x,
             one=yield(1),
             two=yield(2),
             yield(as.numeric(3)))
      yield(y + nchar(x))
    })
    c(nextElemOr(g, NA), nextElemOr(g, NA))
  }

  h("one") %is% c(1, 4)
  h("two") %is% c(2, 5)
  h("twe") %is% c(3, 6)
  h("default") %is% c(3, 10)
  # again contra R's behavior.
  expect_error(h(4), "numeric")
  expect_error(h(2), "numeric")

})

test_that("switch string with fallthrough", {

  h <- function(x) {
    g <- gen({
      y <- switch(x,
                  un=,
                  uno=,
                  one=yield(1),
                  deux=,
                  dos=,
                  two=yield(2),
                  yield(3))
      yield(x)
    })
    paste0(as.character(nextElemOr(g, NA)), nextElemOr(g, NA))
  }

  h("one") %is% "1one"
  h("dos") %is% "2dos"
  h("un") %is% "1un"
  h("vingt") %is% "3vingt"

  # again contra R
  g <- gen(yield(switch("y", x=yield(1), y=, z=)))
  expect_error(nextElemOr(g, NULL), "missing")
})

test_that("numeric switch with delimited goto()", {

  g <- function(x) {
    g <- gen(switch(x,
                    yield("one"),
                    goto(1),
                    goto(5),
                    yield("four"),
                    goto(4)))
    nextElemOr(g, NA)
  }

  g(1) %is% "one"
  g(2) %is% "one"
  g(3) %is% "four"
  g(4) %is% "four"

})


test_that("character switch() with delimited goto()", {

  g <- function(x) {
    gg <- gen({
      switch(x,
             one=yield(1),
             two=goto("one"),
             three=goto("five"),
             four=goto("somewhere else"),
             five=yield(5),
             yield("many"))
    })
    nextElemOr(gg, NA)
  }

  g("one") %is% 1
  g("two") %is% 1
  g("three") %is% 5
  g("four") %is% "many"

})

test_that("Delimited goto with no argument jumps to switch expression", {

  collatz <- function(x) {
    g <- gen({
      yield(x)
      switch(
        if (x == 1) return() else x %% 2 + 1,
        {x <- x / 2;     yield(x); goto()},
        {x <- x * 3 + 1; yield(x); goto()}
      )
    })
    as.numeric(as.list(g))
  }

  length(collatz(31)) %is% 107

})

test_that("Try-finally intercedes with goto", {

  f <- function(x) {force(x); gen({
    switch(x,
           b=yield("four"),
           c=yield(base::stop("nope")),
           a=tryCatch(
           {
             yield("one")
             goto("b")
             yield("nope")
           }, finally={
             yield("three")
           },
           error=yield("nope")
           ))
  })}

  g <- f("a")
  nextElemOr(g, NULL) %is% "one"
  nextElemOr(g, NULL) %is% "three"
  nextElemOr(g, NULL) %is% "four"
  nextElemOr(g, NULL) %is% NULL

})


test_that( "goto from try/catch/finally unwinds the right amount", {

  f <- function(x) {force(x); gen({
    tryCatch(
      switch(x,
             c=yield(base::stop("catchme")),
             aa=,
             a=tryCatch(
             {
               yield("one")
               if (x != "a")
                 base::stop("oops")
               else goto("c")
             },
             error={
               yield("handling inner")
             },
             finally={
               yield("finally")
               base::stop("boops")
             }
             )),
      error=yield("handling outer")
    )
  })}

  g <- f("a")
  nextElemOr(g, NULL) %is% "one"
  nextElemOr(g, NULL) %is% "finally"
  nextElemOr(g, NULL) %is% "handling outer"
  nextElemOr(g, NULL) %is% NULL

  g <- f("aa")
  nextElemOr(g) %is% "one"
  nextElemOr(g) %is% "handling inner"
  nextElemOr(g) %is% "finally"
  nextElemOr(g) %is% "handling outer"
  nextElemOr(g, NULL) %is% NULL

})
