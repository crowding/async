(In progress)

This is an advanced tutorial explaining how the generator
package works, and how to extend it to implement further control
constructs.

# What's a Continuation anyway?

When I first encountered the idea of continuations I was a little
confused by thinking they were some kind of language or runtime
feature, like closures or lifetimes or threading.

Actually, "continuation" is just a name for a functi on call, when it
is made under certain conditions. A continuation is what we call a
function that is called in tail position:

```{r}
ajkl flkj lkj af
```

By a call being in "tail position" we mean that when evaluating the
function 'f', on reaching the call to 'g', 'f' will obligatorily
do whatever 'g' does. By contrast, the function 'h' does not
appear in tail position here, as control returns to 'f' when 'h'
returns.

It is possible for several calls in a function's body to be in tail
positions, for example:

Calls made in tail position are called "continuations" because 

The notion of a _continuation_ starts with the observation that as
soon as you collect the inputs to a function calle d in tail position,
you might as well forget about the function that called it; the
calling function has no more role to play. While we normally think of
'g' returning its value back to 'f', and 'f' returning its value back
to its caller, we might as well think of calling 'g' and then
proceeding from 'g' on to wherever 'f' was going to return to; so the
activation record and environment for `f` is no longer needed.

Programming languages that automatically detect whether a call is
being made in tail position, and preemptively clean up resources, are
said to do "tail call elimination." R does not do this, not least
because lazy evaluation means that R functions are effectively
receiving references to their caller's activation records. However the
concept of continuation is still useful with a workaround.

Whether tailcall elimination is done or not, functions called in tail
position are called continuations, because the 

# The GOTO of functional programming

# Continuation _passing_: the ON C GOTO of functional programming

## Constant continuation-passing functions

## greater_than()

## if() in CPS

## while()

# Manual tail-call elimination (the pump)

# Unwinding and winding

## Try-catch

# Syntactic translation

This is the last part, and conceptually the easiest. As you've seen,
the code to wire together a network of continuation-passing functions
to implement some R syntax, is syntactically isomorphic to the R code
itself. All you need to do is replace any occurrences of "yield" with
a CPS call, replace anything that calls a CPS call, and insulate
anything else behind `arg_cps`. This just consists of changing the
names to point to the right arguments, which task is performed by
`generators:::cps_translate`.

