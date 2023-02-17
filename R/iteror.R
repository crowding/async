#' An efficient and compact iteration protocol.
#'
#' To create an iteror, call the constructor `iteror` providing either
#' a vector or a function as argument. The returned object will
#' support the method `nextOr(obj, or)` to extract successive
#' values.
#'
#' The main method for "iteror" is "nextOr" rather than
#' "nextElem". Instead of using exceptions, "nextOr" uses a lazily
#' evaluated "or" argument to signal the end of iteration.  The "or"
#' argument will only be forced when end of iteration is reached; this
#' means the consumer can provide an action like "break", "next" or
#' "return" to take at the the end of iteration. Summing over an
#' iteror this way looks like:
#'
#' ```
#' sum <- 0
#' it <- iteror(in)
#' repeat {
#'   val <- nextOr(iter, break)
#'   sum <- sum + val;
#' }
#' ```
#'
#' Another way to use the "or" argument is to give it a sigil value;
#' that is, a special value that will be interpreted as end of
#' iteration.  If the result of calling `nextOr` is `identical()`
#' to the sigil value you provided, then you know the iterator has
#' ended. In R it is commonplace to use `NULL` or `NA`, in the role of
#' a sigil, but that only works until you have an iterator that needs
#' to yield NULL. A safer alternative is to use a one-shot sigil
#' value; the result of `new.env()` will work, as it returns a value
#' that by construction is not [identical] to any other object in
#' the R session. This pattern looks like:
#'
#' ```
#' sum <- 0
#' stopped <- new.env()
#' repeat {
#'   val <- nextOr(iter, stopped)
#'   if (identical(val, stopped)) break
#'   sum <- sum + val
#' }
#' ```
#'
#' @export
#' @param obj An object to iterate with. If `obj` is a vector, the
#'   iterator will go over the elements of that vector and you can use
#'   `recycle`.  If `obj` is a function, that function will be called
#'   to compute successive elements. This function should have a
#'   leading argument named `or` and behave accordingly (only forcing
#'   and returning `or` to signal end of iteration.)  If you provide a
#'   function that does not have an `or` argument, you will need to
#'   specify either `catch` or `sigil`.
#' @param ... extra parameters specific to class methods.
#' @return `iteror(obj)` returns an object of class `c('iteror',
#'   'iter')`.
#'
iteror <- function(obj, ...) {
  UseMethod("iteror")
}

#' @exportS3Method
iteror.iteror <- function(obj, ...) obj

#' @exportS3Method iteror iter
iteror.iter <- function(obj, ...) obj

#' @exportS3Method iteror "function"
#' @rdname iteror
#' @param catch If `obj` is a function without an `or` argument, specify
#'   e.g. `catch="StopIteration"` to interpret errors with that
#'   message as end of iteration.
#' @param sigil If `obj` is a function without an `or` argument, specify
#'   which value to watch for end of iteration. Stop will be signaled
#'   if the function result is [identical()] to `sigil`.
iteror.function <- function(obj, ..., catch, sigil) {
  if ("or" %in% names(formals(obj))) {
    fn <- obj
  } else {
    if (!missing(sigil)) {
      force(sigil)
      fn <- function(or) {
        x <- obj(); if (identical(x, sigil)) or else x
      }
    } else if (!missing(catch)) {
      force(catch)
      fn <- function(or) {
        tryCatch(obj(), error=function(e) {
          if (identical(e, message)) {
            or
          } else stop(e)
        })
      }
    } else {
      stop("iteror: must have 'or' argument or else specify 'catch' or 'sigil'")
    }
  }
  structure(list(nextOr=fn), class=c("funiteror", "iteror", "iter"))
}


#' @exportS3Method
#' @rdname iteror
#' @param recycle If `obj` is a vector, and `recycle` is TRUE, the
#'   iterator will re-cycle the elements of `obj` without stopping.
iteror.default <- function(obj, ..., recycle=FALSE) {
  if (is.function(obj)) {
    iteror.function(obj, ...)
  } else {
    i <- 0
    n <- length(obj)
    if (recycle) {
      x <- iteror.function(function(or, ...) {
        i <<- i %% n + 1
        obj[[i]]
      }, ...)
    } else {
      x <- iteror.function(function(or, ...) {
        if (i < n) {
          i <<- i + 1
          obj[[i]]
        } else or
      }, ...)
    }
    x$length <- n
    x$recycle <- recycle
    x$state <- environment(x$nextOr)
    x
  }
}

#' @export
#' @rdname iteror
#' @param obj An [iteror]
#' @param or If the iteror has reached its end, an argument that
#'   will be forced and returned.
#' @return `nextOr` returns the next element in the iteror, or
#'   else forces and returns its `or` argument.
nextOr <- function(obj, or, ...) {
  UseMethod("nextOr")
}

#' @exportS3Method
nextOr.funiteror <- function(obj, or, ...) {
  obj$nextOr(or, ...)
}

#' @exportS3Method iterators::nextElem iteror
nextElem.iteror <- function(obj, ...) {
  nextOr(obj, stop("StopIteration"), ...)
}

#' @exportS3Method
format.iteror <- function(x, ...) {
  "<iteror>"
}

# `sigil()` creates a unique value, with an optional name attached.
# An object created with `sigil()` with compare [`identical()`] to
# itself and to no other object in the R session.
# Sigil values are useful as the "or" argument to `awaitOr` or
# `nextOr`;
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
nextOr.iter <- function(obj, or, ...) {
  # :( this means that if you use nextOr over a regular iter, you
  # are setting up and tearing down a tryCatch in each iteration...
  tryCatch(
    iterators::nextElem(obj),
    error=function(e)
      if (!identical(conditionMessage(e), 'StopIteration')) stop(e) else or)
}

#' @exportS3Method
ihasNext.iteror <- function(iter, ...) {
  noValue <- sigil("noValue")
  endIter <- sigil("endIter")
  last <- noValue
  structure(function(or, query="next", ...) {
    switch(query,
           "next"={
             if (identical(last, noValue))
               last <<- nextOr(iter, endIter)
             if (identical(last, endIter))
               or
             else
               last %then% (last <<- noValue)
           },
           "has"={
             if (identical(last, noValue))
               last <<- nextOr(iter, endIter)
             !identical(last, endIter)
           },
           stop("unknown query: ", query)
           )
  }, class=c("ihasNextOr", "iteror", "ihasNext", "iter"))
}

#' @exportS3Method iterators::nextElem ihasNextOr
nextElem.ihasNextOr <- function(obj, ...) {
  obj(stop("StopIteration", call.=FALSE), query="next", ...)
}

#' @exportS3Method
nextOr.ihasNextOr <- function(obj, or, ...) {
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
    item <- nextOr(x, break)
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

#' Limit the number of elements emitted by an iterator.
#' @param it an [iteror] os something that converts to an iteror.
#' @param n How many elements to take.
#' @return a new [iteror] wrapping the provided one, which will stop
#'   after the specified number of elements.
#' @export
ilimit <- function(it, n) {
  it <- iteror(it)
  n <- as.integer(n)
  i <- 0L
  iteror(function(or, ...) {
    if (i >= n) or else {
      i <<- i + 1L
      nextOr(it, or)
    }
  })
}

#' Iterator equivalent of [seq].
#' @param from The starting value of the sequence.
#' @param to The value to stop at.
#' @param by How much to increment at each step.
#' @return a new [iteror] which will yield the sequence specified.
#' @export
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

#' Iteror that chains multiple arguments together into a single iterator
#'
#' Generates an [iteror] that returns elements from the first argument until it
#' is exhausted. Then generates an iterator from the next argument and returns
#' elements from it. This process continues until all arguments are exhausted
#' Chaining is useful for treating consecutive sequences as a single sequence.
#'
#' @export
#' @param ... multiple arguments to iterate through in sequence
#' @return iteror that iterates through each argument in sequence
#'
#' @examples
#' it <- ichain(1:3, 4:5, 6)
#' as.list(it)
#'
