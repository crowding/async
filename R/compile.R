
#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#' @title Compile a generator into a more efficient form.
#' @param x a [generator](gen) object
#' @param level The compilation level. Currently implemented levels are:
#'   0: Do nothing.
#' @return A newly constructed generator. It may or may not share state variables with the input; using the input generator after compiling is not supported.
#' @author Peter Meilstrup
#'
compile <- function(x, level, ...) {
  UseMethod("compile")
}

