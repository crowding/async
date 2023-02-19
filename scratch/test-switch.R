
# Switch is tricky to implement/compile well because base R "switch" is funky!

first <- capture.output(
  local({
  yield <- function(x) {cat(x, "\n"); x}
  for (i in seq(0, 7, 1)) {
    x <- switch(
      i %% 4 + 1,
      yield("one"),
      yield("two"),
      yield("three")
      #and nothing for four?!
    )
    switch(
      x,
      "one" =, "three" = yield("!"),
      yield(".")
    )
  }
  })
)

fb <- gen({
  for (i in seq(0, 7, 1)) {
    x <- switch(
      i %% 4 + 1,
      yield("one"),
      yield("two"),
      yield("three")
      #and nothing for four?
    )
    x <- x %||% "whoops"
    switch(
      x,
      "one" =, "three" = yield("!"),
      yield(".")
    )
  }
})

as.character(as.list(fb)) %is% first
