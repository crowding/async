# what would the "compiled" state machine form of clapping music end
# up looking like?

# first, "clapping music" in generators:
library(iterators)

gen_claps <- function() gen({
  repeat {
    for (i in c(3, 2, 1, 2)) {
      for (j in 1:i) yield(1)
      yield(0)
    }
  }
})

gen_skip_after <- function(iter, howmany) {
  list(iter, howmany)
  gen({
    repeat {
      for (i in 1:howmany) {
        yield(nextElem(iter))
      }
      nextElem(iter)
    }
  })
}

gen_add2gether <- function(iter1, iter2) {
  list(iter1, iter2)
  gen(repeat(yield(nextElem(iter1) + nextElem(iter2))))
}

gen_collect <- function(iter, n) {
  out <- rep(0, n)
  for (i in 1:n) out[i] <- nextElem(iter)
  out
}

# now a "hand-compiled" state machine version (skipping iterators)
claps <- function() {
  nonce <- (function() function () NULL)()
  .iter <- NULL
  .ix <- NULL
  i <- NULL

  .iter2 <- NULL
  .ix2 <- NULL
  .stop2 <- NULL
  j <- NULL

  .yielded <- NULL
  .state <- 1

  yield <- function(val) .yielded <<- val

  .nextElem <- function() {
    while(is.null(.yielded) && !is.null(.state)) {
      switch(.state,
      { #1
        .iter <<- c(3,2,1,2)
        .ix <<- 1
        .stop <<- length(.iter)
        .state <<- 2
      },
      { #2
        if (.ix <= .stop) {
          i <<- .iter[.ix]
          .ix <<- .ix + 1
          .state <<- 3
        } else {
          .iter <<- NULL
          .state <<- 1
        }
      },
      { #3
        .iter2 <<- 1:i
        .ix2 <<- 1
        .stop2 <<- length(.iter2)
        .state <<- 4
      },
      { #4
        if (.ix2 <= .stop2) {
          j <<- .iter2[.ix2]
          .ix2 <<- .ix2 + 1
          .state <<- 5
        } else {
          .iter2 <<- NULL
          .state <<- 6
        }
      },
      { #5
        yield(1)
        .state <<- 4
      },
      { #6
        yield(0)
        .state <<- 2
      })
    }
    if (is.null(.yielded)) stop("StopIteration")
    else reset(.yielded, .yielded <<- NULL)
  }
  itertools::new_iterator(.nextElem)
}


skip_after <- function(iter, howmany) {
  .iter <- NULL
  .ix <- NULL
  .last <- NULL

  .yielded <- NULL
  .state <- 1

  .nextElem <- function() {
    while(is.null(.yielded) && !is.null(.state)) {
      switch(reset(.state, state <<- NULL),
      { #1
        .iter <<- 1:howmany
        .ix <<- 1
        .last <<- length(.iter)
        .state <<- 2
      },
      { #2
        if(.ix <= .last) {
          i <<- .iter[.ix]
          .yielded <<- nextElem(iter)
          .ix <<- .ix + 1
          .state <<- 2
        } else {
          .iter <<- NULL
          nextElem(iter)
          .state <<- 1
        }
      })
    }
    if (is.null(.yielded)) stop("StopIteration")
    else reset(.yielded, .yielded <<- NULL)
  }
  itertools::new_iterator(.nextElem)
}

add2gether <- function(iter1, iter2) {
  a <- NULL
  b <- NULL
  .yielded <- NULL
  .state <- 1

  .nextElem <- function() {
    while(is.null(.yielded) && !is.null(.state)) {
      switch(reset(.state, state <<- NULL),
      { #1
        .yielded <<- nextElem(iter1) + nextElem(iter2)
        .state <<- 1
      })
    }
    if (is.null(.yielded)) stop("StopIteration")
    else reset(.yielded, .yielded <<- NULL)
  }
  itertools::new_iterator(.nextElem)
}

collect <- function(iter, n) {
  out <- rep(0, n)
  for (i in 1:n) {
    out[i] <- nextElem(iter)
  }
  out
}

# our third comparison is to compute clapping music in a direct loop
# without any iterators.
direct_clap <- function(n) {
  out <- rep(0, n)

  store <- function() {
    iout <- 1
    function(x) {
      out[iout] <<- out[iout] + x
      iout <<- iout + 1
      if (iout > n) stop()
    }
  }

  clap <- function(yield) {
    try(
      repeat {
        for (i in c(3, 2, 1, 2)) {
          for (j in 1:i)
            store(1)
          store(0)
        }
      }, silent=TRUE)
  }

  skipping <- function(sto, after) {
    iin <- 1
    function(x) {
      if (iin > after) {
        iin <<- 1
        #skip
      } else {
        sto(x)
        iin <<- iin+1
      }
    }
  }

  clap(store())
  clap(skipping(store(), 12))
  out
}

direct_time <- system.time(direct_clap(144*13*10))
state_iter <- add2gether(claps(), skip_after(claps(), 144))
state_time <- system.time(collect(state_iter, 144*13*10))
gen_iter <- gen_add2gether(gen_claps(), gen_skip_after(gen_claps(), 144))
gen_time <- system.time(collect(gen_iter, 144*13*10))
## > direct_time
## user  system elapsed 
## 0.001   0.000   0.001 
## > gen_time
## user  system elapsed 
## 64.478   0.390  65.421 
## > state_time
## user  system elapsed 
## 1.291   0.073   1.370

# So this give us some sort of idea the speedup possible with better
# compilation. I thought that moving the setup ahead of time would
# give more of a speedup though.

direct_startup <- system.time(for (i in 1:1000) direct_clap(1))
state_startup <- system.time(for(i in 1:1000) nextElem(add2gether(claps(), skip_after(claps(), 144))))
gen_startup <- 10*system.time(for (i in 1:100) nextElem(gen_add2gether(gen_claps(), gen_skip_after(claps(), 144))))

## > direct_startup
## user  system elapsed 
## 0.546   0.007   0.556 
## > state_startup
## user  system elapsed 
## 0.169   0.002   0.173 
## > gen_startup
## user  system elapsed 
## 13.51    0.38   14.02 


# strange that the state machine is faster than running the bare function once!

# experiment with the performance of switch vs. array of handlers?
switching <- function(n) {
  tot <- 0
  for(i in 1:n) {
    branch <- ceiling(runif(1, 0, 100))
    switch(branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch,
           tot <- tot + branch)
  }
  tot
}


bare <- function(n) {
  tot <- 0
  for (i in 1:n) {
    tot <- tot + ceiling(runif(1, 0, 100))
  }
  tot
}

calling <- function(n) {
  tot <- 0
  branches <- rep(list(NULL), 100)
  for (i in 1:length(branches)) {
    branches[[i]] <- (function(inc) {force(inc); function() tot <<- tot + inc})(i)
  }
  for (i in 1:n) {
    branches[[ceiling(runif(1,0,100))]]()
  }
  tot
}

calling2 <- function(n) {
  tot <- 0
  branches <- rep(list(NULL), 100)
  for (i in 1:length(branches)) {
    branches[[i]] <- (function(inc) {force(inc); function() inc})(i)
  }
  for (i in 1:n) {
    tot <- tot + branches[[ceiling(runif(1,0,100))]]()
  }
  tot
}

system.time(switching(10000))
system.time(bare(10000))
system.time(calling(10000))

# So yeah, calling isn't slow.
