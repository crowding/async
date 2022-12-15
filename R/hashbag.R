## Hashbag
# is a multi-level dictionary type thing.

hashbag <- function() {
  structure(new.env(parent=emptyenv()), class="hashbag")
}

#' @export
`[[.hashbag` <- function(x, ..., ifnotfound=structure(list(), class="hashbag")) {
  indices <- list(...)
  if (!nullish(x) && exists(indices[[1]], envir=x)) {
    if (length(indices) > 1) {
      do.call(`[[`, c(list(get(indices[[1]], envir=x)), indices[-1]))
    } else {
      get(indices[[1]], envir=x)
    }
  } else {
    ifnotfound
  }
}
#' @export
`[[<-.hashbag` <- function(x, ..., value) {
  indices <- list(...)
  if (nullish(x)) x <- hashbag()
  if (length(indices) > 1) {
    if (exists(indices[[1]], envir=x)) {
      val <- x[[ indices[[1]] ]]
    } else {
      val <- hashbag()
    }
    val <- do.call("[[<-", c(quote(val), indices[-1], list(value=value)))
  } else {
    val <- value
  }
  assign(indices[[1]], val, envir=x)
  x
}

all_indices <- function(x) UseMethod("all_indices")

#' @export
all_indices.hashbag <- function(x) {
  l <- as.list.environment(x, all.names=TRUE, sorted=TRUE)
  n <- names(l)
  indices <- mapply(n, l, SIMPLIFY=FALSE, USE.NAMES=FALSE, FUN=function(k, v) {
    if (!nullish(v) && "hashbag" %in% class(v)) {
      mapply(`c`, k, all_indices(v), USE.NAMES=FALSE, SIMPLIFY=FALSE)
    } else list(k)
  })
  concat(indices)
}

#' @exportS3Method as.list hashbag
as.list.hashbag <- function(x, ..., all.names=TRUE) {
  lapply(structure(as.character(names(x)), names=names(x)), function(ix) {
    item <- get(ix, envir=x)
    if ("hashbag" %in% class(item)) {
      as.list(item, all.names=all.names)
    } else item
  })
}
