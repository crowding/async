## -----------------------------------------------------------------------------
collatz <- async::gen(function(x) {
  yield(x)
  while (x > 1) {
    x <- if (x %% 2 == 0) x / 2L else 3L * x + 1
    yield(x)
  }
})

## -----------------------------------------------------------------------------
ctz <- collatz(12)
ctz <- collatz(12)
nextOr(ctz, NA)
nextOr(ctz, NA)
nextOr(ctz, NA)
nextOr(ctz, NA)
nextOr(ctz, NA)

## -----------------------------------------------------------------------------
collatz(27L) |> as.list() |> as.numeric()
#try collatz(63728127L) |> as.list() |> as.numeric()...

## ---- eval=FALSE--------------------------------------------------------------
#  async({
#    for (i in 1:5) {
#      #delay() uses later::later()
#      await(delay(10))
#      cat("Beep", i, "\n")
#      beepr::beep(2)
#    }
#  })

## ---- eval=FALSE--------------------------------------------------------------
#  library(future)
#  library(dplyr)
#  plan(multiprocess(workers=2))
#  
#  url <- "http://analytics.globalsuperhypermegamart.com/2020/March.csv.gz"
#  dest <- "March.csv.gz"
#  
#  dataset <- async({
#    if(!file.exists(dest)) {
#      await(future({
#        cat("Downloading\n")
#        download.file(url, dest)
#      }))
#    }
#    data <- await(future({
#      cat("Parsing\n")
#      read.csv(dest) |>
#      mutate(time = hms::trunc_hms(time, 60*60)) |>
#      group_by(time) |>
#      summarize(sales=sum(amount))
#    }))
#  })
#  
#  # When the data is ready, plot it (in the main process:)
#  async({
#    await(dataset) |>
#    ggplot(aes(time, n)) +
#      xlab("Time") +
#      ylab("Sales")
#  })

## -----------------------------------------------------------------------------
walk <- stream({
  for (i in 1:10)
    for (step in c("left", "right")) {
      yield(step)
      await(delay(0.5))
    }
})

chewGum <- stream(for (i in 1:12) {
  yield("chew")
  await(delay(0.8))
})

printEach <- async(function(st) {
  for (each in st) {cat(each, ", ", sep="")}
  cat("\n")
})

all <- combine(walk, chewGum) |> printEach()
async:::wait_for_it()

