test_that("lazy channel", {

  mc <- mock_lazy_channel()
  expect_error(mc$emit("a"), "active")

  p1 <- nextOr(mc$ch, NULL)
  mc$currentArgs() %is% list()

  #debug(attr(mc$ch, "methods")$nextThen)
  p2 <- nextOr(mc$ch, NULL, foo="bar")
  mc$currentArgs() %is% list()
  expect_resolves_with(p1, "a", mc$emit("a"))
  mc$currentArgs() %is% list(foo="bar")

  expect_resolves_with(p2, "b", mc$emit("b"))
  expect_emits(mc$ch, "c", mc$emit("c"))
  expect_error(mc$emit("A"), "active")
  expect_channel_rejects(mc$ch, "sdfghj", mc$reject("sdfghj"))
  expect_channel_finishes(mc$ch)

})

test_that("combining lazy channels", {

  ch1 <- mock_channel()
  ch2 <- mock_lazy_channel()
  out <- ch_combine(ch1$ch, ch2$ch)
  expect_emits(out, "bar", ch1$emit("bar"))
  expect_emits(out, "baz", ch2$emit("baz"))
  ch1$finish()
  expect_channel_finishes(out, ch2$finish())

})

test_that("summarize / format channel", {

  ch1 <- mock_lazy_channel()
  format(ch1$ch) %is%
    "<Channel (sleeping): 0 awaiting, 0 sent>"
  pr1 <- then(ch1$ch(), toupper)
  wait_for_it()
  format(ch1$ch) %is%
    "<Channel (awaiting): 0 awaiting, 0 sent>"
  pr2 <- then(ch1$ch(), tolower)
  format(ch1$ch) %is%
    "<Channel (awaiting): 1 awaiting, 0 sent>"

  ch1$emit('baz')
  wait_for_it()
  summary(ch1$ch) %is% list(state="awaiting", outgoing=0, awaiting=0, sent=1)

  ch1$emit('qux')
  wait_for_it()
  summary(ch1$ch) %is% list(state="sleeping", outgoing=0, awaiting=0, sent=2)

})
