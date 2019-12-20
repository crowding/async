Keywords: from Python, we have 

generator expressions:
(x**2 for x in my_list if x > 10)

This translates straightforwardly to
genr(for (i in my_list) if(x > 10) x)

Note that this means there is 

which is different from!

genr(for (i in my_list) if (x > 10) yield(x))

Which is to say, we want to yield every expression?

This is proably more suited to Python though.

# The `gen()` macro

`gen()` calls 

# How does it work?

The expression given to `generator()` is scanned for calls to generator keywords
such as `yield()` or `yield_from()`. The enclosing calls are rewritten in
continuation-passing-style, with "_cps" appended to the call name.

Thus a generator function like this:

for <- generator(function(name, list, body) {
    name <- nseval::arg_expr(name)
    body <- nseval::arg_expr(body)
    for (i in src) {
        
    }
})

`generator_()` with the underscore, assumes that you have already done the CPS transformation.
for <- generator_(

)


# CPS style

A prototypical function written in CPS style looks like this:


blocked()

## What about asynchronous iterators? hasNext()?

## what about break? throw?

break and error should be given CPS implementations, as well as tryCatch and friends.

gen({
x <- 10;
loop {
    
}
})

## What is the whole list of a baseline cps implementation?

All these control flow operators...

* switch
* loop
* for
* if
* while
* return
* tryCatch
* break
* next
* ||
* &&
* withRestarts
* NOT "function" (that's what yield_from is for), so function bodies should be
  ignored in the transform??? Or maybe we do want generators with inner functions 
  that yield to the outer genreator?

THis is 
## How to propagate exceptions?

An exception that should be captured and forwarded to the next person who
calls next(), I suppose.

## signal?? withRestarts???

## Use package functions as sigils i.e. StopIteration, Yielded?

The convention used for _cps functions is a double function call. The
first call establishes the conventional argument list, here
`iter`. The second call has one argument, the continuation function,
labeled `cont`. The continuation function takes two arguments. One is
the a function, and one is a continuation.

```{r}
function(n, skip, max) {
i <- 0;
loop {
    i <- i + 1;
    if (i %% skip == 0) next()
    if (i > max) break
    yield(i)
}
}
```

```{r}
loop_cps <- function(n, skip, max) function(cont) 
brace_cps(
_cps(i <- 0);
loop_cps(
brace_cps(wrap_cps(i <- i + 1),
          if_cps (i %% skip == 0,
                  next_cps(), ),
          if_cps (i %% skip == 0,
                  next_cps(), ),
```

# Just usage by "library" seems bad. Seems like you should be able to
# generators::gen() even it you haven't imported all the _cps functions...

That is to say, 

Returning(i, cont)

## Tail recursion and staying off the stack?

In Lisp, CPS syle is just tail-recursion, which means each CPS
function calls the next. In R we don't have tailcall elimination, so
we will emulate it by thunking.  Each CPS function should return the
continuation to call next.

is effectively rewritten like this:

for <- function(name, list, body) {
    name <- nseval::arg_expr(name)
    body <- nseval::arg_expr(body)
    for_cps(i, list, {

    })
})

This means that the repertiore of control-flow operators can be extended; all the extension writer needs to do is implenent
