`%is%` <- expect_equal

test_that("translating munged names in function bodies", {

  callTranslations <- c(f = "barf", g = "hork")
  varTranslations <- c(f = "fff", g = "ggg",
                       x = "what", y = "where")

  tenv <- translationEnv(callTranslations, varTranslations)

  expect_equal(
    translateWithEnv(
      quote( f(g, x) + g(f, y) + f::g(x, quote(y)) ),
      tenv),
    quote( barf(ggg, what) +
             hork(fff, where) +
             f::g(what, quote(y)) ))

  expect_equal(
    trans(
      quote( f(g, x) + g(f, y) + f::g(x, quote(y)) ),
      callTranslations, varTranslations),
    quote( barf(ggg, what) +
             hork(fff, where) +
             f::g(what, quote(y)) ))

})
