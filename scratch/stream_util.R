accumulate <- function(ch) { force(ch)
  stream({
    total <- 0
    repeat yield(total <- total + awaitNextOr(ch, break))
  })
}

total <- function(ch) { force(ch)
  async({
    total <- 0
    for (i in ch) total <- total + i
    total
  })
}
