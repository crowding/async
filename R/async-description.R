#' The async package.
#'
#' The `async` package allows you to write sequential-looking code that can
#' pause, return control to R, then pick up where it left off.
#' Async constructs include generators and async/await blocks.
#'
#' A generator runs until it yields a value and then stops, returning
#' control to R until another value is requested. An async block can
#' pause and return control to R until some data is available, then
#' resume. Generators implement the [iterators::iterators-package]
#' interface, while async blocks implement the [promises::promise]
#' interface.
#'
#'  * [gen(...)] creates a generator (an iterator); within a generator use
#'    [yield(x)] to return a value.
#'  * [async(...)] greates an async block (a promise); within the `async`
#'    write [await(x)] to pause on `x` (another promise).
#'
#' @name async-package
#' @import utils
#' @author Peter Meilstrup
NULL

