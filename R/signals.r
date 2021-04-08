# arg_cps interfaces between normal R evaluation and the CPS interpreter.
#
# It saves its arg, unevaluated, and feeds it to the continuation function.
arg_cps <- function(x) {
  x <- arg(x)
  function(cont, ..., wind) {
    trace(where <- "arg_cps inner")
    ## If we have been handed a "wind" function, use that
    if (is_missing(wind)) {
      do(cont, x)
    } else {
      trace(where <- "arg_cps winding...")
      wind(function() do(cont, x))
    }
  }
}

try_cps <- function(expr, silent=arg_cps(FALSE),
                    outfile=arg_cps(getOption("try.outFile", default = stderr()))) {
  list(expr, silent)
  function(cont, ret, ..., wind) {
    break_to <- cont
    got_silent <- function(val) {
      silent <<- val
      trace(where <- "try_cps got silent")
      outfile(got_outfile, ret=ret, ..., wind=wind)
    }
    got_outfile <- function(val) {
      trace(where <- "try_cps got outfile")
      outfile <<- val
      # all we are doing is adding a "wind" to our state args. It is arg_cps
      # that will actually invoke the "wind"
      expr(cont, ret=ret, ..., wind=new_wind)
    }
    # so what is "windup?" this:
    wind_try <- function(to_try, ...) {
      trace(where <- "winding up")
      tryCatch(to_try(...), error=function(e) {
        trace(where <- "caught")
        # (copied from base::try) {{{
        call <- conditionCall(e)
        if (!is.null(call)) {
          if (identical(call[[1L]], quote(doTryCatch)))
            call <- sys.call(-4L)
          dcall <- deparse(call)[1L]
          prefix <- paste("Error in", dcall, ": ")
          LONG <- 75L
          sm <- strsplit(conditionMessage(e), "\n")[[1L]]
          w <- 14L + nchar(dcall, type = "w") + nchar(sm[1L],
                                                      type = "w")
          if (is.na(w))
            w <- 14L + nchar(dcall, type = "b") + nchar(sm[1L],
                                                        type = "b")
          if (w > LONG)
            prefix <- paste0(prefix, "\n  ")
        }
        else prefix <- "Error : "
        msg <- paste0(prefix, conditionMessage(e), "\n")
        .Internal(seterrmessage(msg[1L]))
        if (!silent && isTRUE(getOption("show.error.messages"))) {
          cat(msg, file = outFile)
          .Internal(printDeferredWarnings())
        }
        retval <- invisible(structure(msg, class = "try-error", condition = e))
        # }}} (copied)
        ret(break_to, retval)
      })
    }

    # If there is already a wind, wrap it
    if (is_missing(wind)) {
      new_wind <- wind_try
    } else {
      new_wind <- function(to_try, ...) {
        trace(where <- "wrapped windup")
        to_try(wind_try, cont, ...)
      }
    }

    silent(got_silent, ret=ret, wind=wind, ...)
  }
}


tryCatch_cps <- function(expr, ..., finally) {
  list(expr, finally)
  handlers <- list(...)
  function(cont, ret, ...) {
    
  }
}

