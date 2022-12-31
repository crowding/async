
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


# how to handle nextElem

nextElem <- function(promise,
                     or=quote(.stopIteration.)error("StopIteration")) {

}

await <- function(prom, or=reject(err), reject=function(err)) stop(err)) {

}


x <- for_cps("",
                            try_cps("", R("", i)),
                            R("", NULL),
                            R("", NULL))
debug(x)
pump(x)

  expect_error(pump(for_cps("",
                            try_cps("", R("", i)),
                            R("", NULL),
                            R("", NULL))), "xpected")
