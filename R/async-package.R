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
#' `async.verbose`: (default FALSE) if TRUE, coroutines will print an extremely
#'   chatty play-by-play of what they are doing.
#' `async.compileLevel`: (0) Default compile level for new coroutines. See
#'   description of levels under [gen].
#' `async.paranoid`: (FALSE) If true, perform various time-consuming integrity
#'   checks on the results of compilation. Meant to be enabled during certain
#'   package tests.
#' `async.destructive`: (TRUE) If true, tear down interpreted coroutines
#'   while building compiled replacements.
#' `async.sendLater`: (TRUE) If true, channels will send messages to listeners
#'   in the event loop. If false, messages are sent immediately, which may be
#'   faster but may have a higher risk of stack overflow, as well as expose
#'   different sorts of bugs in your code.
#'
#' @import utils
#' @author Peter Meilstrup
#' @keywords internal
#' @aliases async-package
#' @name async-package
"_PACKAGE"

trace_ <- function(x) if(getOption("async.verbose")) cat(x)
notrace_ <- function(x) NULL

.onLoad <- function(lib, pkg) {
  options(async.compileLevel=getOption("async.compileLevel") %||% 0,
          async.paranoid=getOption("async.paranoid") %||% FALSE,
          async.verbose=getOption("async.verbose") %||% FALSE,
          async.destructive=getOption("async.verbose") %||% TRUE,
          async.sendLater=getOption("async.verbose") %||% TRUE)
}

## usethis namespace: start
## usethis namespace: end
NULL
