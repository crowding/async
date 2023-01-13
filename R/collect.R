collect_tree <- function(fn) {
  size <- 32
  this_level <- vector("list", length=size)
  levels <- rep(list(this_level), size)
  indices <- rep(1, length=size)
  cur_level <- 1
  cur_ix <- 1
  result <- NULL
  yield <- function(val, name=NULL) {
    #cat("Yield: ", deparse(val), "\n")
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
      yield(v, name)
    }
    v
  }
  fn(yield, open, close)
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
  collect_tree(function(yield, open, close) {
    visit <- function(tree, name="") {
      if (is.sequence(tree)) {
        open()
        for (i in seq_along(tree)) {
          visit(tree[[i]], names(tree)[[i]])
        }
        close(typeof(tree), name)
      } else {
        yield(filter(tree), name)
      }
    }
    visit(tree)
    close()
  })
}

#' Execute a function and collect a set of values by callback.
#'
#' `collect` calls the function in its argument, supplying a callback
#' `yield(val, name=NULL)`. Each value passed to `yield` is collected
#' and the list of all values is returned after `fn` returns.
#'
#' @param fn A function, which should accept a single argument "yield".
#' @param type A prototype output vector (i.e. a vector of the
#'   same type as the desired output, similar to [vapply].) Defaults
#'   to `list()`.
#' @return A vector of the same type.
#' @author Peter Meilstrup
#'
#' @examples
#'
#' #cumulative sum
#' cumsum <- function(vec) {
#'   total <- 0
#'   collect(type=0, function(yield) {
#'     for (i in vec) total <- yield(total+i)
#'   }
#' }
collect <- function(fn, type=list()) {
  size <- 64
  a <- vector(mode(type), length=size)
  i <- 0
  fn(function(val, name=NULL) {
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
    a[[i]] <<- val
  })
  length(a) <- i
  a
}
