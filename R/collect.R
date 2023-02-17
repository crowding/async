collect_tree <- function(fn) {
  size <- 32
  this_level <- vector("list", length=size)
  levels <- rep(list(this_level), size)
  indices <- rep(1, length=size)
  cur_level <- 1
  cur_ix <- 1
  result <- NULL
  emit <- function(val, name=NULL) {
    if (cur_level < 1) stop("Has been closed")
    if (cur_ix > length(this_level))
      length(this_level) <<- min(cur_ix, 2*length(this_level))
    this_level[[cur_ix]] <<- val
    if (!is.null(name)) {
      if (is.null(names(this_level))) names(this_level) <- ""
      names(this_level)[cur_ix] <<- name
    }
    cur_ix <<- cur_ix + 1
    val
  }
  open <- function() {
    #cat("Open \n")
    if (cur_level < 1) stop("Has been closed")
    levels[[cur_level]] <<- this_level
    indices[[cur_level]] <<- cur_ix
    cur_level <<- cur_level + 1
    cur_ix <<- 1
    this_level <<- levels[[cur_level]]
    levels[[cur_level]] <<- list()
  }
  close <- function(type="list", name=NULL, attrs=NULL) {
    #cat("Close: ", deparse(name), "\n")
    switch(type,
           language=, call=
                        v <- as.call(this_level[seq_len(cur_ix-1)]),
           v <- as.vector(this_level[seq_len(cur_ix-1)], type))
    attributes(v) <- attrs
    levels[[cur_level]] <<- this_level
    cur_level <<- cur_level-1
    if(cur_level == 0) {
      result <<- v
    } else {
      this_level <<- levels[[cur_level]]
      cur_ix <<- indices[cur_level]
      emit(v, name)
    }
    v
  }
  fn(emit, open, close)
  while (cur_level > 1) {warning("Should have closed"); close()}
  if (cur_level > 0) close()
  result
}

is.sequence <- function(x)
  switch(typeof(x),
         list= ,
         expression= ,
         language= TRUE,
         FALSE)

tree_filter <- function(tree, filter) {
  collect_tree(function(emit, open, close) {
    visit <- function(tree, name="") {
      if (is.sequence(tree)) {
        open()
        for (i in seq_along(tree)) {
          visit(tree[[i]], names(tree)[[i]])
        }
        close(typeof(tree), name)
      } else {
        emit(filter(tree), name)
      }
    }
    visit(tree)
    close()
  })
}

#' Execute a function and collect a set of values by callback.
#'
#' `collect` calls the function `fn` in its argument, supplying a
#' callback of the form `function (val, name=NULL).` I like to call it
#' `emit`.  While `fn` is running, it can call `emit(x)` any number of
#' times.  After `fn` returns, all the values passed to `emit` are
#' returned in a vector, with optional names.
#'
#' @param fn A function, which should accept a single argument, here
#'     called `emit`.
#' @param type A prototype output vector (i.e. a vector of the same
#'   type as the desired output, similar to `FUN.VALUE` of [vapply].)
#'   Defaults to `list()`.
#' @return A vector of all values passed to `emit` whle `fn` was
#'   running.
#' @author Peter Meilstrup
#'
#' @examples
#'
#' #cumulative sum
#' cumsum <- function(vec) {
#'   total <- 0
#'   collect(type=0, function(emit) {
#'     for (i in vec) total <- emit(total+i)
#'   })
#' }
#'
#' # we could implement `iterors::as.list.iteror` thus:
#' as.list.iteror <- function(it) {
#'   collect(\(yield) repeat yield(nextOr(it, break)))
#' }
collect <- function(fn, type=list()) {
  collector(function(yield, extract) {fn(yield); extract(TRUE)}, type)
}

#' @export
#' @rdname collect
#' @description `collector()` works similarly but does not gather
#'   values when your inner function returns. Instead, it provides
#'   your inner function with two callbacks, one to add a value and
#'   the second to extract the value; so you can use that callback to
#'   extract values at a later time. For an example of `collector`
#'   usage see the definition of [gather].
collector <- function(fn, type=list()) {
  size <- 64
  a <- vector(mode(type), length=size)
  i <- 0
  yield <- function(val, name=NULL) {
    force(val)
    i <<- i + 1
    if (i >= size) {
      size <<- min(2 * size, i)
      length(a) <<- size
    }
    if (!is.null(name)) {
      if(is.null(names(a)))
        names(a) <- ""
      names(a)[[i]] <<- name
    }
    if (is.null(val)) {
      a[i] <<- list(val)
    } else {
      a[[i]] <<- val
    }
  }
  extract <- function(reset=FALSE) {
    if(reset) {
      tmp <- a[seq_len(i)]
      a <<- vector(mode(type), length=size)
      i <<- 0
      tmp
    } else {
      a[seq_len(i)]
    }
  }
  fn(yield, extract)
}

#' Gather all values emitted by a channel.
#'
#' `gather` takes a [channel] as argument and returns a [promise]. It
#' is analogous to what [as.list] does for [iteror]s. All values
#' emitted by the channel will be collected into a list (or other
#' vector.) When the source channel closes, the promise will resolve
#' with the collected list.
#' @export
#' @param ch a [channel] object.
#' @param type Optionally provide a vector of the desired output type
#'   (similarly to using `vapply`); defaults to `list()`
gather <- function(ch, type=list()) {
  promise(\(resolve, reject) {
    collector(type=type, \(yield, extract) {
      subscribe(ch, yield, reject, \() resolve(extract(TRUE)))
    })
  })
}
