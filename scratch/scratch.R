
# For example, take the function

## x <- 0
## while(x <= 100) {x <- x + 1}
## cat(x)

# Pretend we did not have a "while" loop, or any looping construct in
# our language. How could we write a "while" loop?

## function() { if (x <= 100) {x <<- x + 1;} }

## x <- 0
## check <- function(continue) if (x <= 100) continue(TRUE) else continue(FALSE)
## doWhile <- function(checker, body, continue) {
##   doCheck <- function
##   gotCheck <- function(result) if (result) body(loop) else
## }

## doWhile(function(val, continue) continue(x <= 100),
##         function(continue) continue(x <= 100)

f <- function(...) stop("this!")


g <- function(x, ...) tryCatch(
  x(...),
  error = function(...) {
    cat("Caught ", deparse(c(...)), "\n")
    stop(...)
  }
)

g(g, f)

lapply(names(graph$nodeProperties))

cnodes <- lapply(names(graphc$nodes), function(nodeName) {
  structure(list(graphc$nodeProperties[[nodeName]]$"localName"), names=nodeName)
})

name_my_context <- function() {
  contextName <- name.my.context(a)
  # up the frame stack from outer? Is it possible to determine?
}

# Illustrating the difference between parent.frame() and caller()
where <- "global"

test <- function() {
  where <- "test"
  f(g())
  cat(where, "'s caller() is ", caller()$where,
      ", and parent is ", sys.frame(-1)$where, "\n")
}

f <- function(x) {
  x
  where <- "f"
  cat(where, "'s caller() is ", caller()$where,
      ", and parent is ", sys.frame(-1)$where, "\n")
}

g <- function() {
  where <- "g"
  browser()
  cat(where, "'s caller() is ", caller()$where,
      ", and parent is ", sys.frame(-1)$where, "\n")
}

selfnamed <- function(x) structure(x, names=x)
lapply(selfnamed(names(graph$nodes)), \(x)graph$nodeProperties[[x]]$localName)


await <- function(promise, or=error(err), error=stop) {

}

# how to handle nextElem

nextElem <- function(promise,
                     or=error("StopIteration")) {

}

contextLabelMe <- function() {
  # assemble together context labels from the parents?
  parents <- lapply(sys.frames(), function(x) x$contextLabel) %||% character(0)
  current <- as.character(get_call(caller()))
  c(recursive=TRUE, parents, sep=".")
}


   a <- async({
      tryCatch({
        if(FALSE) await(NULL)
        return(2)
        not_run <<- FALSE
      }, finally={
        cleanup <<- TRUE
      })
      not_run <<- FALSE
      5
   })

g <- gen({
  tryCatch({
    if(FALSE) yield(NULL)
    return(2)
    not_run <<- FALSE
  }, finally={
    cleanup <<- TRUE
  })
  not_run <<- FALSE
})

asyncOpts(verbose=TRUE)
g <- gen({
  try({
    yield(5)
    stop("foo")
    yield(6)
  }, silent=TRUE)
  yield(7)
  stop("bar")
  yield(8)
}, compileLevel=-1)

expect_equal(nextElem(g), 5)
expect_equal(nextElem(g), 7)
expect_error(nextElem(g), "bar")
expect_error(nextElem(g), "StopIteration")
