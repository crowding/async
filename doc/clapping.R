## -----------------------------------------------------------------------------
print_pattern <- function(counts = c(3, 2, 1, 2), repeats=4) {
  for (i in seq_len(repeats)) {
    for (c in counts) {
        for (j in 1:c)
          cat(1)
        cat(0)
    }
  }
  cat("\n")
}

## -----------------------------------------------------------------------------
print_pattern(repeats=4);

## -----------------------------------------------------------------------------
library(async)
library(iterors)
counts <- c(3, 2, 1, 2)
pattern <- gen({
  repeat {
    for (n in counts) {
      for (j in 1:n)
        yield(1)
      yield(0)
    }
  }
})

## -----------------------------------------------------------------------------
for (i in 1:24) {
  cat(nextOr(pattern, break))
}
cat("\n")

## -----------------------------------------------------------------------------
gen_pattern <- gen(function(counts = c(3, 2, 1, 2)) {
  repeat {
    for (n in counts) {
      for (j in 1:n)
        yield(1)
      yield(0)
    }
  }
})

## -----------------------------------------------------------------------------
show_head <- function(x, n=24) {
  x |> as.list(n=n) |> deparse() |> cat(sep="\n")
}
show_head(gen_pattern(), 24)

## ---- eval=FALSE--------------------------------------------------------------
#  tmp <- tempdir()
#  baseurl <- "https://github.com/octoblu/drum-kit/raw/master/public/assets/samples"
#  samplepaths <- paste0(tmp, c("x" = "/clap4.wav","X" = "/clap5.wav"))
#  curl::curl_download(paste0(baseurl, "/clap%20(4).WAV"), samplepaths[1])
#  curl::curl_download(paste0(baseurl, "/clap%20(5).WAV"), samplepaths[2])

## ---- eval=FALSE--------------------------------------------------------------
#  library(audio) # for load.wave, play
#  claps <- lapply(samplepaths, load.wave)
#  play(claps[[1]])
#  play(claps[[2]])

## ---- eval=FALSE--------------------------------------------------------------
#  gen_pattern() |> ilimit(36) |> iplay(claps, 360)

## -----------------------------------------------------------------------------
iapply <- function(it, f, ...) { list(it, f, ...)
  iteror(function(or) {
    f(nextOr(it, return(or)), ...)
  })
}

isink <- function(it, then=invisible(NULL)) {
  repeat nextOr(it, break)
  then
}

## -----------------------------------------------------------------------------
g <- gen_pattern() |> ilimit(24) |> iapply(cat) |> isink(cat("\n"))

## -----------------------------------------------------------------------------
gapply <- gen(function(it, f, ...) for (x in it) yield(f(x, ...)) )
gsink <- function(it, then=NULL) {run(for (i in it) NULL); then}

## -----------------------------------------------------------------------------
drop_one_after <- gen(function(g, n, sep=character(0)) {
  repeat {
    for (i in 1:n) yield(nextOr(g, break))
    nextOr(g, break) #drop
    cat(sep) # print a seperator after every skip
  }
})

## -----------------------------------------------------------------------------
iseq() |>
ilimit(12) |>
drop_one_after(3, "\n") |>
iapply(cat, "") |>
isink()

## -----------------------------------------------------------------------------
clapping_music <- function(n=12, counts=c(3,2,1,2), sep=" ") {
  cell <- sum(counts+1) # how long?
  a <- gen_pattern(counts)
  b <- gen_pattern(counts) |> drop_one_after(n*cell, sep)
  # add them together and limit the output
  gen(for (i in 1:(n*(cell+1)*cell)) {
    x <- nextOr(a, break)
    y <- nextOr(b, break)
    yield(x+y)
  })
}

## -----------------------------------------------------------------------------
clapping_music(4, sep="\n") |> iapply(cat) |> isink(cat("\n"))

## ---- eval=FALSE--------------------------------------------------------------
#  iplay(clapping_music(n=4, sep="\n"), claps, 480)

