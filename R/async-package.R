#' The async package.
#'
#' The `async` package allows you to write sequential-looking code
#' that can pause, return control to R, then pick up where it left
#' off.  Async constructs include generators, async/await blocks, and
#' streams (experimental as of async 0.3).
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
#' Theare are some global package [options]:
#'
#' `async.verbose`: if TRUE, coroutines will print an extremely chatty
#'   play-by-play of what they are doing.
#' `async.compileLevel`: Default compile level for new generators. See
#'   [gen].
#' `async.paranoid`: if TRUE, perform various time-consuming integrity
#'   checks on the results of compilation. Meant to be enabled during
#'   package tests.
#' `async.destructive`: If TRUE (default), tear down interpreted coroutines
#'   while building compiled replacements.
#'
#' @import utils
#' @author Peter Meilstrup
#' @keywords internal
#' @aliases async-package
#' @name async-package
"_PACKAGE"

.onLoad <- function(lib, pkg) {
  options(async.compileLevel=0,
          async.paranoid=FALSE,
          async.verbose=FALSE,
          async.destructive=TRUE)
}

trace_ <- function(x) if(getOption("async.verbose")) cat(x)

## usethis namespace: start
## usethis namespace: end
NULL
