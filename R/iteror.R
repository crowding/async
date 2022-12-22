# The "iterators" package uses stop("StopIteration") and tryCatch to
# signal end of iteration, but tryCatch has a lot of overhead. In the
# context of a generator, when you are in a "for" loop over an
# iterator, you have to be setting up and tearing down the trycatch on
# each iteration. so that you can return control from the generator.
#
# The main method for "iteror" is "nextElemOr" rather than
# "nextElem". Instead of exceptions, "nextElemOr" uses a lazily
# evaluated "or" argument to signal the end of iteration.  The "or"
# argument is lazily evaluated, and will only be forced at the stop of
# iteration; this means the consumer can provide a "break" or "return"
# to respond to the end of the loop.
#
# sum <- 0
# it <- iteror(in)
# repeat {
#   val <- nextElemOr(iter, break)
#   sum <- sum + val;
# }
#
# Compare with the usual technique of consuming an iterator with
# `nextElem()`:
#
# sum <- 0
# it <- iter(in)
# tryCatch(
#   repeat {
#     val <- nextElem(iter)
#     sum <- sum + val
#   },
#   error=function(x) if (!iteration_has_ended(e)) stop(e)
# )
#
# Another way to use the "or" argument is to give it a sigil value;
# that is, a value that you know will not appear in the values yielded
# by the generator. If the result of nextElemOr is this sigil value,
# then you know the iterator has ended. In R it is commonplace to use
# `NULL` as a sigil, but you do sometimes want to have an iterator
# return literal `NULL`s. A more general way is to use a one-time
# sigil value; the function `sigil()` returns a nonce, i.e. a new
# value that is not [`identical()`] to any other object in the R session.
#
# stopped <- sigil()
# sum <- 0
# repeat {
#   i <- nextElemOr(iter, stopped)
#   if (identical(i, stopped)) break
#   sum <- sum + i
# }

#' @export
iteror <- function(it, ...) {
  UseMethod("iteror")
}

#' @export
iteror.iteror <- identity

#' @export
iteror.iter <- identity

#' @export
iteror.function <- function(obj, ...) {
  if (!("or" %in% names(formals(obj)))) stop("iteror: must have 'or' argument")
  structure(list(nextElemOr=obj), class=c("funiteror", "iteror", "iter"))
}

#' @export
iteror.default <- function(obj, ...) {
  i <- 0
  n <- length(obj)
  iteror.function(function(or, ...) {
    if (i < n) {
      i <<- i + 1
      obj[[i]]
    } else or
  }, ...)
}

#' @export
nextElemOr <- function(obj, or, ...) {
  UseMethod("nextElemOr")
}

#' @export
nextElemOr.funiteror <- function(obj, or, ...) {
  obj$nextElemOr(or, ...)
}

#' @importFrom iterators nextElem
NULL
#' @export
nextElem.iteror <- function(obj, ...) {
  nextElemOr(obj, stop("StopIteration"), ...)
}

# `sigil()` creates a unique value, with an optional name attached.
# An object created with `sigil()` with compare [`identical()`] to
# itself and to no other object in the R session.
# Sigil values are useful as the "or" argument to `awaitOr` or
# `nextElemOr`;
# @return a closure; calling the closure returns the name.
sigil <- function(name=NULL) function()name

ihasNext <- function(obj, ...) {
  UseMethod("ihasNext")
}

hasNext <- function(obj, ...) {
  UseMethod('hasNext')
}

#' @exportS3Method
ihasNext.ihasNextOr <- identity

#' @exportS3Method
ihasNext.default <- function(obj) ihasNext(iteror(obj))

`%then%` <- function(a, b) { force(a); force(b); a }

#' @exportS3Method
nextElemOr.iter <- function(iter, or) {
  # :( this means that if you use nextElemOr over a regular iter, you
  # are setting up and tearing down a tryCatch in each iteration...
  tryCatch(
    nextElem(iter),
    error=function(e)
      if (!identical(conditionMessage(e), 'StopIteration')) stop(e) else or)
}

#' @exportS3Method
ihasNext.iteror <- function(obj, ...) {
  noValue <- sigil("noValue")
  endIter <- sigil("endIter")
  last <- noValue
  structure(function(or, query="next", ...) {
    switch(query,
           "next"={
             if (identical(last, noValue))
               last <<- nextElemOr(obj, endIter)
             if (identical(last, endIter))
               or
             else
               last %then% (last <<- noValue)
           },
           "has"={
             if (identical(last, noValue))
               last <<- nextElemOr(obj, endIter)
             !identical(last, endIter)
           },
           stop("unknown query: ", query)
           )
  }, class=c("ihasNextOr", "iteror", "ihasNext", "iter"))
}

#' @exportS3Method
nextElem.ihasNextOr <- function(obj, ...) {
  obj(stop("StopIteration", call.=FALSE), query="next", ...)
}

#' @exportS3Method
nextElemOr.ihasNextOr <- function(obj, or, ...) {
  obj(or, query="next", ...)
}

#' @exportS3Method
hasNext.ihasNextOr <- function(obj, ...) {
  obj(query="has", ...)
}

#' @exportS3Method as.list iteror
as.list.iteror <- function(x, n=as.integer(2^31-1), ...) {
  size <- 64
  a <- vector('list', length=size)
  i <- 0
  repeat {
    item <- nextElemOr(x, break)
    i <- i + 1
    if (i >= size) {
      size <- min(2 * size, n)
      length(a) <- size
    }
    a[[i]] <- item
  }
  length(a) <- i
  a
}

icount <- function(count) {
  i <- 0L
  if(missing(count)) {
    iteror(function(or) {
      i <<- i + 1L
    })
  } else {
    iteror(function(or) {
      if (i >= count) or else i <<- i + 1L
    })
  }
}

ilimit <- function(iterable, n) {
  it <- iteror(iterable)
  n <- as.integer(n)
  i <- 0L
  iteror(function(or, ...) {
    if (i >= n) or else {
      i <<- i + 1L
      nextElemOr(it, or)
    }
  })
}

bench_iterators <- function() {
  # holy hell what the heck is iter() DOING??? 10x overhead!?
  # ah, it is switching depending on whether it's a function(n) or not.
  # and recycle() and that all takes some time.
  # they should specialize at construction time and return a closure.
  microbenchmark(
    as.list(iter(1:500)),
    as.list(iter(local({
      x <- 0;
      function() if (x < 500) x <<- x + 1 else stop("StopIteration")
    }))),
    as.list(new_iterator(local({
      x <- 0;
      function() if (x < 500) x <<- x + 1 else stop("StopIteration")
    }))),
    as.list(iteror(local({
      x <- 0;
      function(or) if (x < 500) x <<- x + 1 else or
    }))),
    as.list(iteror(1:500)),
    as.list(ihasNext(icount(500))),
    as.list(ihasNextOr(icountor(500))),
    as.list(icount(500)),
    as.list(icountor(500)),
    as.list(ilimit(icount(), 500)),
    as.list(ilimitor(icountor(), 500)))
}

filter.iteror <- function(it, predicate) {
  iteror(function(or) {
    repeat
      if (predicate(x <- nextElem(it, return(or))))
        return(x)
  })
}

imap <- function(it, fn) {
  iteror(function(or) {
    repeat {
      x <- nextElemOr(it, return(or))
      return(fn(x, next))
    }
  })
}

ichain <- function(its) {
  its <- iteror(its)
  it <- emptyIteror()
  iteror(function(or) {
    repeat {
      return(nextElemOr(it, {
        it <<- iteror(nextElemOr(its, return(or)))
        next
      }))
    }
  })
}

chain <- function(...) {
  ichain(list(...))
}

izip <- function(...) {
  iterors <- lapply(list(...), iteror)

  if (length(iterors) == 0) return(emptyIteror())

  iteror(function(or) {
    lapply(iterors, nextElemOr, return(or))
  })
}

emptyIteror <- function() iteror(function(or) or)

accumulate <- function(it, func=`+`) {
  sum <- 0
  iteror(function(or) sum <<- func(sum, nextElemOr(it, return(or))))
}

iseq <- function(from=1L, to=Inf, by=1L) {
  i <- from - by
  if (by > 0L)
    iteror(function(or) {
      i <<- i + by
      if (i > to) or else i
    })
  else
    iteror(function(or) {
      i <<- i + by
      if (i < to) or else i
    })
}
