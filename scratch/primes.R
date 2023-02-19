# Generator performance explorations

# Here is an example of what might be a horrid performance case:
# a generator that yields prime numbers, via trial division. Written
# obnoxiously with the "yield" inside a double loop.
compiler::setCompilerOptions(optimize=3)
compiler::enableJIT(3)

coro_primes <- function(n) {
  coroprimes <- coro::gen({
    yield(2)
    yield(3)
    i <- 3
    repeat {
      i <- i + 2
      j <- 3
      repeat {
        if ( i %% j == 0 ) {
          break
        }
        if (j >= sqrt(i)) {
          yield(i)
          break
        }
        j <- j + 2
      }
    }
  })
  collect_co(coroprimes, n)
}

coro_generator <- coro::generator({
    yield(2)
    yield(3)
    i <- 3
    repeat {
      i <- i + 2
      j <- 3
      repeat {
        if ( i %% j == 0 ) {
          break
        }
        if (j >= sqrt(i)) {
          yield(i)
          break
        }
        j <- j + 2
      }
    }
  })
  collect_co(coroprimes, n)
}

collect_co <- function(coro, n) {
  out <- numeric(n)
  for (i in 1:n) {
    out[i] <- coro()
  }
  out
}

gen_primes <- function(n) {
  genprimes <- gen({
    yield(2)
    yield(3)
    i <- 3
    repeat {
      i <- i + 2
      j <- 3
      repeat {
        if ( i %% j == 0 ) {
          break
        }
        if (j >= sqrt(i)) {
          yield(i)
          break
        }
        j <- j + 2
      }
    }
  })
  collect(genprimes, n)
}

collect <- function(iter, n) {
  out <- numeric(n)
  for (i in 1:n) {
    out[i] <- nextElem(iter)
  }
  out
}

# hand-coded state-machine iterator following the same algorithm
# i.e. what the above could compile to
iter_primes <- function(n) {
  iterprimes <- iter(local({
    state <- "two"
    i <- NULL
    j <- NULL
    yielding <- NULL
    function() {
      yielding <<- NULL
      while(is.null(yielding)) {
        switch(state,
               two={
                 yielding <<- 2
                 state <<- "three"},
               three={
                 yielding <<- 3
                 i <<- 3
                 state <<- "next_i"},
               next_i={
                 i <<- i+2
                 j <<- 3
                 state <<- "check"},
               check={
                 if(i %% j == 0) {
                   state <<- "next_i"
                 } else if( j >= sqrt(i) ) {
                   yielding <<- i
                   state <<- "next_i"
                 } else {
                   state <<- "next_j"
                 }
               },
               `next_j`={
                 j <<- j+2
                 state <<- "check"
               },
               stop("illegal state"))
      }
      yielding
    }
  }))
  collect(iterprimes, n)
}

# same state machine running in sequential code rather than iterator
list_primes_enclos <- function(n) {
  out <- numeric(n)
  state <- "two"
  i <- NULL
  j <- NULL
  yielding <- NULL
  listprimes <- function() {
    state <<- "two"
    for (i.out in 1:n) {
      yielding <<- NULL
      while(is.null(yielding)) {
        switch(state,
               two={
                 yielding <<- 2
                 state <<- "three"},
               three={
                 yielding <<- 3
                 i <<- 3
                 state <<- "next_i"},
               next_i={
                 i <<- i+2
                 j <<- 3
                 state <<- "check"},
               check={
                 if(i %% j == 0) {
                   state <<- "next_i"
                 } else if( j >= sqrt(i) ) {
                   yielding <<- i
                   state <<- "next_i"
                 } else {
                   state <<- "next_j"
                 }
               },
               `next_j`={
                 j <<- j+2
                 state <<- "check"
               },
               stop("illegal state"))
      }
      out[i.out] <<- yielding
    }
  }
  listprimes()
  out
}

# using $ instead of <<-
list_primes_slot <- function(n) {
  private <- as.environment(list(
    out=numeric(100), state="two", i=NULL, j=NULL, yielding=NULL))
  listprimes <- function(private) {
    for (i.out in 1:n) {
      private$yielding <- NULL
      while(is.null(private$yielding)) {
        switch(private$state,
               two={
                 private$yielding <- 2
                 private$state <- "three"},
               three={
                 private$yielding <- 3
                 private$i <- 3
                 private$state <- "next_i"},
               next_i={
                 private$i <- private$i+2
                 private$j <- 3
                 private$state <- "check"},
               check={
                 if(private$i %% private$j == 0) {
                   private$state <- "next_i"
                 } else if( private$j >= sqrt(private$i) ) {
                   private$yielding <- private$i
                   private$state <- "next_i"
                 } else {
                   private$state <- "next_j"
                 }
               },
               `next_j`={
                 private$j <- private$j+2
                 private$state <- "check"
               },
               stop("illegal state"))
      }
      private$out[i.out] <- private$yielding
    }
    private
  }
  listprimes(private)
  private$out
}

getNext <- compiler::compile(quote({
  yielding <- NULL
  while(is.null(yielding)) {
    switch(state,
           two={
             yielding <- 2
             state <- "three"},
           three={
             yielding <- 3
             i <- 3
             state <- "next_i"},
           next_i={
             i <- i+2
             j <- 3
             state <- "check"},
           check={
             if(i %% j == 0) {
               state <- "next_i"
             } else if( j >= sqrt(i) ) {
               yielding <- i
               state <- "next_i"
             } else {
               state <- "next_j"
             }
           },
           `next_j`={
             j <- j+2
             state <- "check"
           },
           stop("illegal state"))
  }
  yielding
}))

# keep state and locals in same env and eval state loop in that env
list_primes_eval <- function(n) {
  e <-(function(out=numeric(n), i.out=1, state="two", i=NULL, j=NULL,
                yielding=NULL) environment())()
  out <- numeric(n)
  for (i.out in 1:n) {
    out[i.out] <- eval(getNext, e)
  }
  out
}

# state machine without <<-
list_primes_singlenv <- function(n) {
  out <- numeric(n)
  i.out <- 1
  state <- "two"
  i <- NULL
  j <- NULL
  yielding <- NULL
  for (i.out in 1:n) {
    yielding <- NULL
    while(is.null(yielding)) {
      switch(state,
             two={
               yielding <- 2
               state <- "three"},
             three={
               yielding <- 3
               i <- 3
               state <- "next_i"},
             next_i={
               i <- i+2
               j <- 3
               state <- "check"},
             check={
               if(i %% j == 0) {
                 state <- "next_i"
               } else if( j >= sqrt(i) ) {
                 yielding <- i
                 state <- "next_i"
               } else {
                 state <- "next_j"
               }
             },
             `next_j`={
               j <- j+2
               state <- "check"
             },
             stop("illegal state"))
    }
    out[i.out] <- yielding
  }
  out
}

#state machine using numeric switch instead of character
list_primes_numeric_singlenv <- function(n) {
  out <- numeric(n)
  state <- 1
  i <- NULL
  j <- NULL
  yielding <- NULL
  for(i.out in 1:n) {
    yielding <- NULL
    while(is.null(yielding)) {
      switch(state,
             { #1
               yielding <- 2
               state <- 2},
             { #2
               yielding <- 3
               i <- 3
               state <- 3},
             { #3
               i <- i+2
               j <- 3
               state <- 4},
             { #4
               if(i %% j == 0) {
                 state <- 3
               } else if( j >= sqrt(i) ) {
                 yielding <- i
                 state <- 3
               } else {
                 state <- 5
               }
             },
             { #5
               j <- j+2
               state <- 4
             },
             stop("illegal state"))
    }
    out[i.out] <- yielding
  }
  out
}

e <- (function(state=2, i=NULL, j=NULL,
               yielding=NULL) environment())()
getNext_numeric <- compiler::compile(env=e, quote({
  yielding <- NULL
  while(is.null(yielding)) {
    switch(state,
    { #1
      yielding <- 2
      state <- 2},
    { #2
      yielding <- 3
      i <- 3
      state <- 3},
    { #3
      i <- i+2
      j <- 3
      state <- 4},
    { #4
      if(i %% j == 0) {
        state <- 3
      } else if( j >= sqrt(i) ) {
        yielding <- i
        state <- 3
      } else {
        state <- 5
      }
    },
    { #5
      j <- j+2
      state <- 4
    },
    stop("illegal state"))
  }
  yielding
}))
# keep state and locals in same env and eval state loop in that env
list_primes_eval_numeric <- function(n) {
  e <- (function(state=2, i=NULL, j=NULL, yielding=NULL) environment())()
  out <- numeric(n)
  for (i.out in 1:n) {
    out[i.out] <- eval(getNext_numeric, e)
  }
  out
}
list_primes_eval_numeric_fn <- function(n) {
  out <- numeric(n)
  e <- (function(state=2, i=NULL, j=NULL, yielding=NULL) environment())()
  nextFn <- function() eval(getNext_numeric, e)
  out <- numeric(n)
  for (i.out in 1:n) {
    out[i.out] <- nextFn()
  }
  out
}
list_primes_eval_numeric_fn_trycatch <- function(n) {
  out <- numeric(n)
  e <- (function(state=2, i=NULL, j=NULL, yielding=NULL) environment())()
  nextFn <- function() tryCatch(eval(getNext_numeric, e), error=function(e) stop("StopIteration"))
  out <- numeric(n)
  for (i.out in 1:n) {
    out[i.out] <- nextFn()
  }
  out
}
list_primes_eval_numeric_fn_wch <- function(n) {
  out <- numeric(n)
  e <- (function(state=2, i=NULL, j=NULL, yielding=NULL) environment())()
  nextFn <- function() withCallingHandlers(eval(getNext_numeric, e), error=function(e) stop("StopIteration"))
  out <- numeric(n)
  for (i.out in 1:n) {
    out[i.out] <- nextFn()
  }
  out
}
iter_primes_eval_numeric <- function(n) {
  e <- (function(state=2, i=NULL, j=NULL, yielding=NULL) environment())()
  iterprimes <- iter(function() eval(getNext_numeric, e))
  collect(iterprimes, n)
}

nextElem.primeiter <- function(x) tryCatch(x(), error=stop("StopIteration"))
simple_iter_primes_eval_numeric <- function(n) {
  e <- (function(state=2, i=NULL, j=NULL, yielding=NULL) environment())()
  iterprimes <- structure(function() tryCatch(eval(getNext_numeric, e), class="primeiter"), error=   ???)
  collect(iterprimes, n)
}


e <- (function(i=1) environment())()
count <- compiler::compile(env=e, quote(i <- i + 1))
iter_dummy <-function(n) {
  i <- 0
  it <- iter(function() i)
  collect(it, n)
}
iter_count_eval <- function(n) {
  e <- (function(i=1) environment())()
  iterprimes <- iter(function() eval(count, e))
  collect(iterprimes, n)
}

# and the algorithm in "plain" R
r_primes <- function(n.out) {
  out <- rep(list(NULL), n.out)
  i.out <- 1
  yield <- function(x) {out[i.out] <<- x; i.out <<- i.out+1}
  yield(2)
  yield(3)
  i <- 3
  repeat {
    i <- i + 2
    j <- 3
    repeat {
      if ( i %% j == 0 ) {
        break
      }
      if (j >= sqrt(i)) {
        yield(i)
        if (i.out > n.out) return(out)
        break
      }
      j <- j + 2
    }
  }
}

library(microbenchmark)
library(dplyr)

iter_marks <- microbenchmark(
  r_primes(200),
  list_primes_enclos(200),
  list_primes_slot(200),
  list_primes_singlenv(200),
  list_primes_numeric_singlenv(200),
  list_primes_eval(200),
  list_primes_eval_numeric(200),
  list_primes_eval_numeric_fn(200),
  list_primes_eval_numeric_fn_trycatch(200), #"realistic" iterator?
  list_primes_eval_numeric_fn_wch(200),
  collect(iterators::icount(), 200),
  as.list(itertools::ilimit(iterators::icount(), 200)),
  iter_dummy(200),
  iter_count_eval(200),
  iter_primes(200),
  iter_primes_eval_numeric(200),
  simple_iter_primes_eval_numeric(200)
##  gen_primes(200)
)

marks <- microbenchmark( times=10,
  list_primes_eval_numeric_fn(1000),
  coro_primes(1000),
  gen_primes(1000)
)

marks <- microbenchmark( times=100,
  list_primes_eval_numeric_fn(1),
  coro_primes(1),
  gen_primes(1)
)


## Ah, so starting to see a lot of cost from nextElem.funiter
## (as opposed to iterators::icount)
## which uses a lot of slotting I see.
## so maybe I want to implement my own iterator interface...
## and I should add a tryCatch to this just to see...
## cool, so the tryCatch explains a lot.
## I def want to limit myself to a one tryCatch

## So, conclusions from this exercise are, for best generator performance,
## the compiled form should be:
## * states are numbers
## * store temp / state variables in the same local environment
## * execute by eval'ing a compiled expression
## * limit the number of tryCatches you have. Why is tryCatch so
##   much more expensive than withCallingHandlers?

## Unit: milliseconds
## expr                                min         lq       mean     median         uq        max neval
##                     r_primes(200)   1.628690   1.752363   2.401987   1.856426   2.054539   26.13499   100
## collect(iterators::icount(), 200)   1.045381   1.158423   1.725926   1.241928   1.324479   20.45601   100
##                  list_primes(200)   2.886119   3.096847   4.711095   3.311123   4.960477   43.87721   100
##                 list_primes2(200)   2.101407   2.222698   3.444391   2.341925   2.729861   41.74484   100
##                   iter_primes(200)  11.373279  11.873565  14.947899  12.501227  13.563974   62.96386   100
##                     gen_primes(200) 529.835634 561.559702 651.338259 573.350763 647.789172 1433.88579   100
