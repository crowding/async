
#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#' @title Compile a generator into a more efficient form.
#' @param x a [generator](gen) object
#' @param level The compilation level. Currently implemented levels
#'   are:
#'      -1: Do name munging only.
#'       0: Do nothing.
#' Planned levels:
#'       1: Do name munging and generate a state machine.
#'       2. Above plus basic inlining and constant folding
#'       3. Above plus aggressive inlining
#' @return A newly constructed generator. It may or may not share
#'   state variables with the input; using the input generator after
#'   compiling is not supported.
#' @author Peter Meilstrup
#'
compile <- function(x, level, ...) {
  UseMethod("compile")
}

