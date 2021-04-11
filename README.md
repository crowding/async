# Async R

This is an R package inplementing *generators* and *async* blocks

This package is rather experimental.

## Generators

`g <- gen({...})` allow you to write a block of sequential code that
"pauses". A generator runs until it hits a `yield()` call, then
returns the value. The next time you call the generator it picks up
where it left off and runs until the next `yield`.

From the "outside" a generator implements the `iterator`
interface. You extract each yielded value with `nextElem(g)`

```
devtools::install_github("crowding/generators")

# This generator produces a sequence whose length is unknown ahead of time
# https://en.wikipedia.org/wiki/Collatz_conjecture
collatz <- function(x) { force(x)
  generators::gen({
    while (x > 1) {
      x <- if (x %% 2 == 0) x / 2L else 3L * x + 1
      yield(x)
    }
  })
}
collatz(27L) %>% as.list %>% as.numeric
#   [1]   82   41  124   62   31   94   47  142   71  214  107  322  161  484  242
#  [16]  121  364  182   91  274  137  412  206  103  310  155  466  233  700  350
#  [31]  175  526  263  790  395 1186  593 1780  890  445 1336  668  334  167  502
#  [46]  251  754  377 1132  566  283  850  425 1276  638  319  958  479 1438  719
#  [61] 2158 1079 3238 1619 4858 2429 7288 3644 1822  911 2734 1367 4102 2051 6154
#  [76] 3077 9232 4616 2308 1154  577 1732  866  433 1300  650  325  976  488  244
#  [91]  122   61  184   92   46   23   70   35  106   53  160   80   40   20   10
# [106]    5   16    8    4    2    1
collatz(63728127L) %>% as.list %>% as.numeric
```

For more examples, see the "Clapping Music" tutorial in "clapping.r".

## Async/await

Like generators `a <- async({...})` blocks also take a block of
sequential code, which runs until it reaches a call to `await(p)`. The
argument `p` should be a promise, as defined by the `promises`
package; which represents an unfinished external computation. From the
"outside," an `async()` constructs and returns a promise.

An `async` block runs until it reaches a call to `await(p)`; when the
promise `p` resolves with a value. the `async` block continues running.
When the `async` block finishes, it finishes with a value.

## How does this work?

The `gen` and `async` constructors takes the code block you give it,
and then swap out R's base control flow operators like `if`, `while`,
`try`, with its own versions that can execute partially. This is done
using [continuation passing style](http://),` (also affectionately known
as "callback hell".)

So it is essentially an R interpreter written in R, so not yet very
performant. However it is being written with an eye to _compiling_
generators/asyncs into a state machine representation, which should
improve performance substantially.
