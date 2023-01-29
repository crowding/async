# Some generics and methods common to all coroutines

getState <- function(x, ...) UseMethod("getState")
getNode <- function(x, ...) UseMethod("getNode")
getPump <- function(x) UseMethod("getPump")
getEntry <- function(x) UseMethod("getEntry")
getReturn <- function(x) UseMethod("getReturn")
getStop <- function(x) UseMethod("getStop")
getCurrent <- function(x) UseMethod("getCurrent")
getOrig <- function(x) UseMethod("getOrig")
getStartSet <- function(x) UseMethod("getStartSet")

compile <- function(x, level, ...) UseMethod("compile")

#' @exportS3Method
getReturn.coroutine <- function(x) {
  environment(getPump(x))$return_
}
#' @exportS3Method
getStop.coroutine <- function(x) {
  environment(getPump(x))$stop_
}
#' @exportS3Method
getNode.coroutine <- function(x, ...) {
  environment(getPump(x))$getCont()
}
#' @exportS3Method
getEntry.coroutine <- function(x) {
  environment(getPump(x))$entry
}

#' @exportS3Method
print.coroutine <- function(x, ...) {
  cat(format(x, ...), sep="\n")
}

#' @export
debugAsync <- function(x, R, internal) UseMethod("debugAsync")

#' @exportS3Method
debugAsync.coroutine <- function(x, R=current$R, internal=current$internal) {
  sd <- get("setDebug", envir = environment(getPump(x)))
  current <- sd()
  sd(R, internal)
}

#' @exportS3Method
compile.coroutine <- function(x, level) {
   if (abs(level) >= 1) {
    if (paranoid) graph <- walk(x)
    munged <- munge( x )
    if (abs(level) >= 3) {
      stop("TODO: Aggressive inlining")
    } else if (abs(level) >= 2) {
      stop("TODO: Basic inlining/constant folding")
    }
    if (level >= 1) {
      stop("TODO: switch generation")
    } else {
      out <- reconstitute(x, munged)
      if (paranoid) expect_properly_munged(graph, out)
      out
    }
  } else x
}

#' @exportS3Method
getStartSet.coroutine <- function(x) {
  # named nodes that are "entry points" for the graph to collect,
  # their names after munging will be as given.
  # This is the base list; specialized methods add to it.
  list(entry = getEntry(x),
       return_ = getReturn(x),
       stop_ = getStop(x),
       pump = getPump(x),
       runPump = environment(getPump(x))$runPump,
       base_winding = environment(getPump(x))$base_winding,
       setDebug = environment(getPump(x))$setDebug,
       getCont = environment(getPump(x))$getCont)
}

format.coroutine <- function(x, ...) {
  envir <- environment(getPump(x))
  code <- getOrig(x)
  a <- deparse(call(class(x)[[1]], code), backtick=TRUE)
  b <- format(envir, ...)
  state <- getState(x)
  cont <- getNode(x)
  c <- paste0(c("<", class(x)[[1]], " [",
                state,
                " at `", cont, "`",
                if (state=="stopped")
                  c(": ", capture.output(print(envir$err))),
                "]>"), collapse="")
  d <- NextMethod()
  c(a, b, c, d)
}

reconstitute <- function(orig, munged) UseMethod("reconstitute")
