# Generators for R

This is a rather experimental R package as yet.

You may have seen generators in languages like Python, Lua and Elixir,
and this package implements them for R.

Generators are bits of code whose execution can be paused and
restarted; they "look like" for loops on the inside but they act like
iterators on the outside.

```
devtools::install_github("crowding/generators")
# https://en.wikipedia.org/wiki/Collatz_conjecture
collatz <- function(x) { force(x)
  generators::gen({
    while (x > 1) {
      x <- if (x %% 2 == 0) x / 2 else 3 * x + 1
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
