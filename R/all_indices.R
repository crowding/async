all_indices <- function(x, levels=Inf) UseMethod("all_indices")

#' @exportS3Method
all_indices.environment <- function(x, levels=Inf) {
  x <- as.list.environment(x, all.names=TRUE, sorted=TRUE)
  all_indices.list(x, levels=levels)
}

#' @exportS3Method
all_indices.list <- function(x, levels=Inf) {
  n <- names(x)
  indices <- mapply(n, x, SIMPLIFY=FALSE, USE.NAMES=FALSE, FUN=function(k, v) {
    if (!nullish(v) && any(c("environment", "list") %in% class(v)) && levels > 1) {
      mapply(`c`, k, all_indices(v, levels=levels-1),
             USE.NAMES=FALSE, SIMPLIFY=FALSE)
    } else list(k)
  })
  concat(indices)
}
