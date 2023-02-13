---
title: "The `async` package: Generators and async/await for R"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteIndexEntry{The `async` package: Generators and async/await for R}
  %\VignetteEncoding{UTF-8}
---

# The `async` package: Generators, async/await, and asynchronous streams for R

This is an R package implementing *generators*, *async* blocks, and *streams*; (which are collectively known as "coroutines.")

[![](https://www.r-pkg.org/badges/version/async?color=purple)](https://cran.r-project.org/package=async)[![check-standard](https://github.com/crowding/async/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/crowding/async/actions/workflows/check-standard.yaml)
[![test-coverage](https://github.com/crowding/async/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/crowding/async/actions/workflows/test-coverage.yaml)

# New features in version 0.3

* Single step through a coroutine using `debugAsync(obj, R=TRUE)` to inspect at R level, or `debugAsync(obj, internal=TRUE`
* `switch` supports `goto()` to transfer to a different branch
* Coroutines now support `on.exit`
* Experimental implementation of `channel` interface and `stream()` coroutine.
* Coroutines are printed with a label indicating where in their code they are paused

For more details see [NEWS.md]().

## Generators

`g <- gen({...})` allow you to write a block of sequential code that
"pauses". A generator runs until it hits a `yield()` call, then
returns the value. The next time you call the generator it picks up
where it left off and runs until the next `yield`.

[iterators]: https://CRAN.R-project.org/package=iterators

From the "outside" a generator implements the `iteror` interface.  You
extract each yielded value with `nextElemOr(g, ...)`, and you can use
generators anywhere you can use an iterator. The `iteror` class is
cross compatible with the [iterators]() package.

### Example: Collatz sequence

Consider a sequence of numbers `x[i]`, starting with an arbitrary `x[1]`, where
each subsequent element is produced by applying the rule:

* If `x[i]` is even, then the next value will be `x[i+1] = x[i]/2`.
* if `x[i]` is odd,  the next value will be `x[i+1] = 3*x[i]+1`.

[conjectured]: https://en.wikipedia.org/wiki/Collatz_conjecture
An infinite sequence of numbers will continue form each staring point
`x[1]`, but it is [conjectured]() that all sequences will
eventually reach the loop 1, 4, 2, 1, 4, 2, .... The following
generator produces the Collatz sequence, starting from `x`, and
terminating when (or if?) the sequence reaches 1.

```R
collatz <- function(x) { force(x)
  async::gen({
    yield(x)
    while (x > 1) {
      x <- if (x %% 2 == 0) x / 2L else 3L * x + 1
      yield(x)
    }
  })
}
```

The call to `gen` produces a generator. You can get values one at a
time with `nextElemOr()`.

```r
ctz <- collatz(12)
ctz <- collatz(12)
nextElemOr(ctz)
# [1] 12
nextElemOr(ctz)
# [1] 6
nextElemOr(ctz)
# [1] 3
nextElemOr(ctz)
# [1] 10
nextElemOr(ctz)
# [1] 5
```

You can also use any other method that applies to an iterator, like `as.list`.

```r
collatz(27L) |> as.list |> as.numeric
#   [1]   82   41  124   62   31   94   47  142   71  214  107  322  161  484  242
#  [16]  121  364  182   91  274  137  412  206  103  310  155  466  233  700  350
#  [31]  175  526  263  790  395 1186  593 1780  890  445 1336  668  334  167  502
#  [46]  251  754  377 1132  566  283  850  425 1276  638  319  958  479 1438  719
#  [61] 2158 1079 3238 1619 4858 2429 7288 3644 1822  911 2734 1367 4102 2051 6154
#  [76] 3077 9232 4616 2308 1154  577 1732  866  433 1300  650  325  976  488  244
#  [91]  122   61  184   92   46   23   70   35  106   53  160   80   40   20   10
# [106]    5   16    8    4    2    1
collatz(63728127L) |> as.list |> as.numeric
```

For more examples, see the ["Clapping Music" vignette.](https://crowding.github.io/async/articles/clapping.html)

## Async/await

[promises]: https://rstudio.github.io/promises/ "promises"
Like `gen`, `async({...})` takes a block of sequential code, which
runs until it reaches a call to `await(p)`. The argument `p` should be
a promise, (as defined by the [promises] package, which
represents an unfinished external computation.) In turn, `async()`
constructs and returns a promise.

An `async` block runs until it reaches a call to `await(p)` and
pauses.  When the promise `p` resolves, the `async` block continues.
If `p` rejects, that is evaluated like an error; you can put
`await(p)` into a `tryCatch` to handle rejections. When the `async`
block finishes, or throws an error, its promise resolves or rejects.

### Examples:

`async` doesn't handle running parallel tasks by itself; it builds on existing
packages like `future` and `later`. The `later` package lets you
assign tasks to be done in the event loop, when R is idle.

Ring a bell 5 times at 10 second intervals (subject to R being idle):

```r
async({
  for (i in 1:5) {
    await(delay(10))   #delay() uses later::later()
    cat("Beep", i, "\n")
    beepr::beep(2)
  }
})
```

#### Shiny apps

`async()` can be used in Shiny apps! For an example, here is a version
of the ["Cranwhales" demo app using
async/await.](https://github.com/crowding/cranwhales-await).

#### Background processing

`async` can also work with `future` objects to run computations in parallel.
Download, parse, and summarize a dataset in background processes:

```r
library(future)
library(dplyr)
plan(multiprocess(workers=2))

url <- "http://analytics.globalsuperhypermegamart.com/2020/March.csv.gz"
dest <- "March.csv.gz"

dataset <- async({
  if(!file.exists(dest)) {
    await(future({
      cat("Downloading\n")
      download.file(url, dest)
    }))
  }
  data <- await(future({
    cat("Parsing\n")
    read.csv(dest) |>
    mutate(time = hms::trunc_hms(time, 60*60)) |>
    group_by(time) |>
    summarize(sales=sum(amount)) |>
  }))
})

# When the data is ready, plot it (in the main process:)
async({
  await(dataset) |>
  ggplot(aes(time, n)) +
    xlab("Time") +
    ylab("Sales")
})
```
