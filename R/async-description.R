#' The async package.
#'
#' The `async` package allows you to write sequential-looking code that can
#' pause, return control to R, then pick up where it left off.
#' Async constructs include generators and async/await blocks.
#'
#' A generator runs until it yields a value and then stops, returning
#' control to R until another value is requested. An async block can
#' pause and return control to R until some data is available, then
#' resume. Generators implement the [iterator][iterators::iterators-package]
#' interface, while async blocks implement the [promise][promises::promise]
#' interface.
#'
#'  * [`gen(...)`][gen] creates a generator (an iterator); within a generator use
#'    [`yield(x)`][yield] to return a value.
#'  * [`async(...)`][async] creates an async block (a promise); within the `async`
#'    write [`await(x)`][await] to pause on `x` (another promise).
#'
#' @name async-package
#' @import utils
#' @author Peter Meilstrup
NULL

paranoid <- FALSE
verbose <- FALSE
trace_ <- function(x) if(verbose) cat(x)

compileLevel <- 0

#' Get or set global options for the async package.
#'
#' If new settings are provided in arguments they take effect for any new
#' generators or asyncs created.
#' @param verbose If TRUE, coroutines will print an extremely chatty
#'   play-by-play of the steps they are taking.
#' @param compileLevel Default compile level for new generators. See [gen]().
#' @param paranoid if TRUE, perform verious integrity checks on the
#'   result of compilation. Meant to be enabled during testing.
#' @return a list containing the current settings.
#' @export
asyncOpts <- function(
    verbose=get("verbose", parent.env(environment())),
    compileLevel=get("compileLevel", parent.env(environment())),
    paranoid=get("paranoid", parent.env(environment()))) {
  list(verbose, compileLevel, paranoid)
  unlockBinding("compileLevel", getNamespace("async"))
  unlockBinding("verbose", getNamespace("async"))
  unlockBinding("paranoid", getNamespace("async"))
  list(verbose=verbose<<-verbose,
       compileLevel=compileLevel<<-compileLevel,
       paranoid=paranoid<<-paranoid)
}
