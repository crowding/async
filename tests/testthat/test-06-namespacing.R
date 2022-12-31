`%is%` <- expect_equal

test_that("namespacing", {
  x <- async::gen(yield(0))
  nextElem(x) %is% 0
  x <- evalq(async::gen(yield(0)), baseenv())
  nextElem(x) %is% 0
  x <- evalq(async::gen({yield(0); yield(1)}), baseenv())
  nextElem(x) %is% 0
  nextElem(x) %is% 1
})


test_that("Isolated env", {
  g <- eval(quote(async::gen(yield(0))), baseenv())
  nextElem(g) %is% 0

  g <- evalq(async::gen({yield(1); yield(2)}), baseenv())
})

test_that("custom CPS function in isolated env", {

  e <- new.env(parent=baseenv())
  with(e, {
    identity_cps <- function(.contextName, x) {force(x)
      function(cont, ret, ...) {
        x(cont, ret=ret, ...)
      }
    }
    identity <- function(x) x
  })

  g <- evalq(async::gen(yield(identity(yield(5)))), e)
  nextElem(g) %is% 5
  nextElem(g) %is% 5

  f <- new.env(baseenv())
  f$identity = e$identity
  g <- evalq(async::gen(yield(identity(yield(5)))), f)
  nextElem(g) %is% 5
})
