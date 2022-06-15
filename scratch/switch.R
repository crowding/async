
# how do we actually write SWITCH using SWITCH. It's tricky!

switch_cps <- function(EXPR, ...) {
  force(EXPR);
  constructors <- list(...)

  function(cont, ..., stop, trace=trace_) {
    alts <- lapply(constructors,
                   function(c) c(cont, ..., stop=stop, trace=trace))

    # put branch targets into individual varables alt1, alt2, ...altN
    # so that walk() can follow
    for (i in seq_along(alts)) {
      nm <- paste0("alt", i)
      assign(nm, alts[[i]])
      if (is_R(alts[[i]]) && is_missing_(R_expr(i))) {
        alts[[i]] <- missing_value()
      } else {
        alts[[i]] <- as.name(nm)
      }
    }
    # "alts" now holds the names of the branch targets

    # synthesize a switch node that walk() can follow
    switch_ <- qe(function(cont, val) {
      if (is.numeric(val) && ((val < 1) || floor(val) > .(length(alts)))) {
        # I HATE this behavior, it should just stop, but it is what R does.
        cont(invisible(NULL))
      } else {
        switch(
          val,
          ..(alts),
          # if there are names but no default, we need one
          # so that we can continue with invisible(NULL)
          # which, again, I hate but it's what R does.
          # if there are no names, names(alts) == NULL, and !all(NULL) is FALSE
          ..(if (!all(names(alts)) == "") alist(cont(invisible(NULL)))))
      }
    })
  }
}
