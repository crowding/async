`%is%` <- expect_equal

test_that("Tree collector", {

  collect_tree(function(yield, open, close) {
    yield(quote(a))
    yield("one")
    open()
    yield(quote(b))
    yield(2, "val")
    close("call")
    yield("c")
    close("call")
  }) %is% quote(a("one", b(val=2), "c"))

  collect_tree(function(yield, open, close) {
    open()
    open()
    close("list")
    close("language")
    open()
    close("list")
  }) %is% list(as.call(list(list())),list())

})

test_that("tree filter", {

  reverse_names <- function(tree) {
    tree_filter(tree, function(node) {
      switch(typeof(node),
             character=strrev(node),
             symbol=as.name(strrev(as.character(node))),
             logical=!node,
             integer=, double=, complex= -node,
             node
             )
    })[[1]]
  }

  reverse_names(
    quote(this(is, my(1), "test", FALSE))) %is%
    bquote(siht(si, ym(.(-1)), "tset", TRUE))

})


test_that("translating munged names in function bodies", {

  callTranslations <- c(f = "barf", g = "hork")
  varTranslations <- c(f = "fff", g = "ggg",
                       x = "what", y = "where")

  expect_equal(
    trans(
      quote( f(g, x) + g(f, y) + f::g(x, quote(y)) ),
      callTranslations, varTranslations),
    quote( barf(ggg, what) +
             hork(fff, where) +
             f::g(what, quote(y)) ))

  expect_equal(
    transX(
      quote( f(g, x) + g(f, y) + f::g(x, quote(y)) ),
      callTranslations, varTranslations),
    quote( barf(ggg, what) +
             hork(fff, where) +
             f::g(what, quote(y)) ))

  if(FALSE) {

    microbenchmark(
    trans( body(tryCatch),
#      quote( f(g, x) + g(f, y) + f::g(x, quote(y)) ),
      callTranslations, varTranslations),
    transX( body(tryCatch),
 #     quote( f(g, x) + g(f, y) + f::g(x, quote(y)) ),
      callTranslations, varTranslations)
    )

  }

})
