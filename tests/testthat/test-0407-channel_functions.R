test_that("ch_apply", {

  inn <- mock_channel()
  out <- ch_apply(inn$ch, toupper)
  expect_emits(out, "FOO", inn$emit("foo"))
  expect_emits(out, "BAR", inn$emit("bar"))
  expect_channel_rejects(out, "wat", inn$reject("wat"))
  expect_channel_finishes(out, NULL)

})

test_that("combining channels", {

  pr1 <- mock_promise()
  ch1 <- mock_channel()
  pr2 <- mock_promise()

  out <- ch_combine(pr1, ch1$ch, pr2)

  expect_emits(out, "foo", pr1$resolve("foo"))
  expect_emits(out, "bar", ch1$emit("bar"))
  expect_emits(out, "baz", ch1$emit("baz"))
  expect_emits(out, "qux", pr2$resolve("qux"))
  expect_channel_finishes(out, ch1$finish())

})

options(async.sendLater = TRUE)
options(async.compileLevel = 0)
