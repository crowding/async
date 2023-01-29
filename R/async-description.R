#' The async package.
#'
#' The `async` package allows you to write sequential-looking code
#' that can pause, return control to R, then pick up where it left
#' off.  Async constructs include generators, async/await blocks, and
#' streams (experimental as of async 0.3)
#'
#' A generator runs until it yields a value and then stops, returning
#' control to R until another value is requested. An async block can
#' pause and return control to R until some data is available, then
#' resume. Generators implement the [iteror] interface (which is
#' back-compatible with [iterator][iterators::iterators-package]
#' package.) Async blocks implement the [promise][promises::promise]
#' interface. The new [stream] construct implements the [channel]
#' interface, which is defined in this package.
#'
#'  * [`gen(...)`][gen] creates a generator (an iterator); within a generator use
#'    [`yield(x)`][yield] to return a value.
#'  * [`async(...)`][async] creates an async block (a promise); within the `async`
#'    write [`await(x)`][await] to pause on `x` (another promise).
#'  * [`stream(...)`][stream] creates a stream (an asynchronous iterator or
#'    [channel]); in writing a `stream` you can use both `await` and `yield`.)
#'
#' @name async-package
#' @import utils
#' @author Peter Meilstrup
NULL

paranoid <- FALSE
destructive <- TRUE
verbose <- FALSE
trace_ <- function(x) if(verbose) cat(x)

compileLevel <- 0

#' Get or set global options for the async package. These options are
#' mostly used in package development or unit testing.
#'
#' If new settings are provided in arguments they take effect for any
#' new generators or asyncs created.
#' @param verbose if TRUE, coroutines will print an extremely chatty
#'   play-by-play of what they are doing.
#' @param compileLevel Default compile level for new generators. See
#'   [gen]().
#' @param paranoid if TRUE, perform various time-consuming integrity
#'   checks on the result of compilation. Meant to be enabled during
#'   testing.
#' @param destructive defaults TRUE: tear down interpreted coroutines
#'   while building compiled replacements. Used in package debugging.
#' @return a list containing the current settings.
#' @export
asyncOpts <- function(
    verbose=get("verbose", parent.env(environment())),
    compileLevel=get("compileLevel", parent.env(environment())),
    paranoid=get("paranoid", parent.env(environment())),
    destructive=get("destructive", parent.env(environment()))) {
  list(verbose, compileLevel, paranoid, destructive)
  unlockBinding("compileLevel", getNamespace("async"))
  unlockBinding("destructive", getNamespace("async"))
  unlockBinding("verbose", getNamespace("async"))
  unlockBinding("paranoid", getNamespace("async"))
  list(verbose=verbose<<-verbose,
       compileLevel=compileLevel<<-compileLevel,
       paranoid=paranoid<<-paranoid,
       destructive=destructive<<-destructive)
}
