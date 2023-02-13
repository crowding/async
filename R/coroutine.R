# Some generics and methods common to all coroutines.

#' @rdname format
#' @param x a coroutine ([async], [gen], or [stream]) object.
#' @return `getNode()` returns a character naming the last known execution node.
#' @param ... Undocumented.
#' @details
#' The state displayed by `format()` or `getNode()` can be read like an
#' adsress pointing to a spot in the source code; for example, a state
#' string like `.{1.<-2.await__then` can be read like
#' "in the first argument of `{`, in the second argument of `<-`, in a
#' call to `await()`, at internal node `then`."
#'
#' @export
getNode <- function(x, ...) UseMethod("getNode")

#' @export
#' @rdname format
getState <- function(x, ...) UseMethod("getState")

#' @export
#' @rdname format
#' @return `getOrig()` returns the original expression given to the coroutine's
#' constructor.
getOrig <- function(x, ...) UseMethod("getOrig")

#' @export
#' @rdname format
#' @return `getEnv()` returns the coroutine's effective environment.
getEnv <- function(x, ...) UseMethod("getEnv")

# not-exported methods
getPump <- function(x) UseMethod("getPump")
getEntry <- function(x) UseMethod("getEntry")
getReturn <- function(x) UseMethod("getReturn")
getStop <- function(x) UseMethod("getStop")
getCurrent <- function(x) UseMethod("getCurrent")
getStartSet <- function(x) UseMethod("getStartSet")
compile <- function(x, level, ...) UseMethod("compile")
reconstitute <- function(orig, munged) UseMethod("reconstitute")

#' @exportS3Method
getEnv.coroutine <- function(x, ...) {
  environment(getPump(x))
}

#' @exportS3Method
getReturn.coroutine <- function(x) {
  environment(getPump(x))$return_
}

#' @exportS3Method
getStop.coroutine <- function(x) {
  environment(getPump(x))$stop_
}

#' @exportS3Method
#' @rdname format
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

#' Toggle single-step debugging for a coroutine.
#' @param x A coroutine object as constructed by ([async], [gen] or [stream]).
#' @param R set TRUE to step through expressions at user level
#' @param internal Set TRUE to step through at coroutine implementation level.
#' @return a `list(R=, internal=) with the current debug setting.
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
    if (options$paranoid) graph <- walk(x)
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
      if (options$paranoid) expect_properly_munged(graph, out)
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

#' Query / display coroutine properties and state.
#'
#' A coroutine's `format` method displays its pre-compiled source
#'   code, its effective environment, whether it is running or
#'   finished, and a label indicating its last known state. The
#'   methods `getState`, `getNode` and `getOrig` also expose this
#'   information.
#'
#' @exportS3Method
#' @rdname format
format.coroutine <- function(x, ...) {
  envir <- getEnv(x)
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

# Transform a [nseval::quo] of a function definition
# into a coroutine definition, e.g.
coroutine_function <- function(arg, head, ...) {
  extra <- list(...)
  fn <- expr(arg)
  args <- fn[[2]]
  body <- fn[[3]]
  forcer <- as.call(c(list(quote(list)), lapply(names(args), as.name)))
  body <- as.call(list(quote(`{`),
                       forcer,
                       as.call(c(list(head, body, local=FALSE), extra))))
  fn <- call("function", args, body)
  quo_(fn, env(arg))
}
