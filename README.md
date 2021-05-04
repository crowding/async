# Asynchronous R

This is an R package implementing *generators* and *async* blocks.

## Generators

`g <- gen({...})` allow you to write a block of sequential code that
"pauses". A generator runs until it hits a `yield()` call, then
returns the value. The next time you call the generator it picks up
where it left off and runs until the next `yield`.

From the "outside" a generator implements the `iterator`
interface. You extract each yielded value with `nextElem(g)`,
or using the [`iterators`][iterators] or [`itertools`][itertools]
[iterators]: https://cran.r-project.org/web/packages/iterators/index.html
[itertools]: https://itertools.r-forge.r-project.org/

### Example: Collatz sequence

Consider a sequence of numbers `x[i]`, starting with an arbitrary `x[1]`, where
each subsequent element is produced by applying the rule:

* If `x[i]` is even, then the next value will be `x[i+1] = x[i]/2`.
* if `x[i]` is odd,  the next value will be `x[i+1] = 3*x[i]+1`.

An infinite sequence of numbers will continue form each staring point
`x[1]`, but it is [conjectured][conjecture] that all sequences will
eventually reach the loop 1, 4, 2, 1, 4, 2, .... The following
generator produces the Collatz sequence, starting from `x`, and
terminating when (if) the sequence reaches 1.
[conjecture]: https://en.wikipedia.org/wiki/Collatz_conjecture

```{R}
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
time with `nextElem()`

```{r}
ctz <- collatz(12)
> ctz <- collatz(12)
> nextElem(ctz)
[1] 12
> nextElem(ctz)
[1] 6
> nextElem(ctz)
[1] 3
> nextElem(ctz)
[1] 10
> nextElem(ctz)
[1] 5
```

You can also use any other method that applies to an iterator, like `as.list`.

```
collatz(27L) |> as.list |> as.numeric
#   [1]   82   41  124   62   31   94   47  142   71  214  107  322  161  484  242
#  [16]  121  364  182   91  274  137  412  206  103  310  155  466  233  700  350
#  [31]  175  526  263  790  395 1186  593 1780  890  445 1336  668  334  167  502
#  [46]  251  754  377 1132  566  283  850  425 1276  638  319  958  479 1438  719
#  [61] 2158 1079 3238 1619 4858 2429 7288 3644 1822  911 2734 1367 4102 2051 6154
#  [76] 3077 9232 4616 2308 1154  577 1732  866  433 1300  650  325  976  488  244
#  [91]  122   61  184   92   46   23   70   35  106   53  160   80   40   20   10
# [106]    5   16    8    4    2    1
collatz(63728127L) |> as.list %>% as.numeric
```

For more examples, see the "Clapping Music" vignette:

## Async/await

Like `gen`, `async({...})` takes a block of sequential code, which
runs until it reaches a call to `await(p)`. The argument `p` should be
a promise, (as defined by the [promises] package, which
represents an unfinished external computation. In turn, `async()`
constructs and returns a promise.
[promises]: https://rstudio.github.io/promises/ "promises"

An `async` block runs until it reaches a call to `await(p)` and
pauses.  When the promise `p` resolves, the `async` block continues.
If `p` rejects, that is evaluated like an error; you can put
`await(p)` into a `tryCatch` to handle rejections. When the `async`
block finishes, or throws an error, its promise resolves or rejects.

### Example: Pomodoro timer

`Async` doesn't to any multithreading by itself; it builds on existing
packages like `future` and `later`. The `later` package lets things
run in R's packground event loop.

```{R}

```

### Example: Cranwhales

`async()` can be used in Shiny apps! I've ported the "Cranwhales" demo
app to use async/await, here[]
[cranwhales-await]

## How does this work?

The `gen` and `async` constructors takes the code block you give it,
and then swap out R's base control flow operators like `if`, `while`,
`tryCatch`, and so on with its own versions that are "pausable," that
is they can evaluate partially and then resume later. So it is
essentially an interpreter for a subset of R, written in R. Plans are
to turning the interpreter into a compiler...
