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
getReturn.coroutine <- function(x) {
  environment(getPump(x))$return_
}

#' @exportS3Method
getStop.coroutine <- function(x) {
  environment(getPump(x))$stop_
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
#' @param x A coroutine object as constructed by ([async], [gen] or
#'   [stream]).
#' @param R Set TRUE to step through expressions at user level
#' @param internal Set TRUE to step through at coroutine
#'   implementation level.
#' @param trace Set TRUE or provide a print function to print each R
#'   expression evaluated in turn.
#' @return a `list(R=, internal=, trace=)` with the current debug state.
#' @export
debugAsync <- function(x, R, internal, trace) UseMethod("debugAsync")

#' @exportS3Method
debugAsync.coroutine <- function(x, R=current$R, internal=current$internal, trace=current$trace) {
  sd <- get("setDebug", envir = environment(getPump(x)))
  current <- sd()
  sd(R, internal, trace)
}

#' @exportS3Method
compile.coroutine <- function(x, level, ...) {
   if (abs(level) >= 1) {
    if (getOption("async.paranoid")) graph <- walk(x)
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
      if (getOption("async.paranoid")) expect_properly_munged(graph, out)
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
       getPumpState = environment(getPump(x))$getPumpState)
}

#' Query / display coroutine properties and state.
#'
#' The coroutine `format` method displays its source code, its
#'   effective environment, whether it is running or finished, and a
#'   label indicating its last known state. The `summary` method
#'   returns the same information in a list.
#' @param x A coroutine.
#' @exportS3Method
#' @rdname format
format.coroutine <- function(x, ...) {
  s <- summary(x)
  a <- deparse(call(class(x)[[1]], s$code), backtick=TRUE)
  b <- format(s$targetEnv, ...)
  c <- paste0(c("<", class(x)[[1]], " [",
                s$state,
                " at `", s$cont, "`",
                if (s$state %in% c("stopped", "rejected"))
                  c(": ", capture.output(print(s$err))),
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

#' @rdname format
#' @param object a coroutine ([async], [generator][gen], or [stream]) object.
#' @param ... Undocumented.
#' @description `summary(obj)` returns a list with information on a coroutine's state,  including:
#' * `code`: the expression used to create the coroutine;
#' * `state`: the current state (see below);
#' * `node`: is a character string that
#'    identifies a location in the coroutine source code; for example,
#'    a typical state string might be ".\{.<-2.await__then", which
#'    can be read like "in the first argument of `\{`, in the second
#'    argument of `<-`, in a call to `await()`, at internal node `then`.";
#' * `envir`: the environment where the coroutine is evaluating R expressions;
#' * `err`: the error object, if the coroutine caught an error.
#' @export
summary.coroutine <- function(object, ...) {
  s <- environment(getPump(object))$getPumpState()
  d <- debugAsync(object)
  c(s,
    debug=d)
}
