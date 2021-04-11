# what would the "compiled" state machine form of clapping music end
# up looking like?

# first, "clapping music" in generators:

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

# now a manually "compiled" version (skipping iterators)

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
  #itertools::new_iterator(.nextElem)
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
          .yielded <<- iter()
          .ix <<- .ix + 1
          .state <<- 2
        } else {
          .iter <<- NULL
          iter()
          .state <<- 1
        }
      })
    }
    if (is.null(.yielded)) stop("StopIteration")
    else reset(.yielded, .yielded <<- NULL)
  }
  #itertools::new_iterator(.nextElem)
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
        .yielded <<- iter1() + iter2()
        .state <<- 1
      })
    }
    if (is.null(.yielded)) stop("StopIteration")
    else reset(.yielded, .yielded <<- NULL)
  }
  #itertools::new_iterator(.nextElem)
}

collect <- function(iter, n) {
  out <- rep(0, n)
  for (i in 1:n) {
    out[i] <- iter()
  }
  out
}

# third comparison is to compute clapping music without any iterators.
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


dt <- system.time(direct_clap(144*13*10))
st <- system.time(collect(add2gether(claps(), skip_after(claps(), 144)), 144*13*10))

#state_iter <- ilimit(add2gether(claps(), skip_after(claps(), 144)), 144*13)
#st <- system.time({sc <- as.numeric(as.list(state_iter))})

gen_iter <- gen_add2gether(gen_claps(), gen_skip_after(gen_claps(), 144))
gt <- system.time({gc <- gen_collect(gen_iter, 144*13)})

# so state machine could offer a ~100x speedup vs. COS generators, but
# leaves you ~5-10x off of direct, non-incremental speed.

# experiment with the performance of switch vs. array of handlers.
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



