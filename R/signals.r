# arg_cps interfaces between normal R evaluation and the CPS interpreter.
#
# It saves its arg, unevaluated, and feeds it to the continuation function.
arg_cps <- function(x) {
  x <- arg(x)

  function(cont, ..., stop=base::stop) {
    list(cont, stop)
    function() {
      trace("arg: ", deparse(expr(x)))
      tryCatch(do(cont, x), error=stop)
    }
  }
}

return_cps <- function(x) {
  maybe(x)
  function(cont, ..., ret, return) {
    if (is_missing(x)) {
      function() return(NULL) # this is out "return" argument not base return
    } else {
      x(doReturn, ..., ret=ret, return=return)
    }
  }
}

tryCatch_cps <- function(expr, ..., finally) {
  list(expr, ..., maybe(finally))
  function(cont, ..., ret, stop, windup, unwind, return) {
    if (is_missing(finally)) {
      doExpr <- expr(cont, ..., ret, stop=stop_, windup=windup, unwind=unwind, return=return)
      windup(doExpr, ..., ret, stop=stop, windup=windup, unwind=unwind, return=return)
      # stop and return need to unwind...
    }
  }
}

try_cps <- function(expr, silent=arg_cps(FALSE),
                    outfile=arg_cps(getOption("try.outFile", default = stderr()))) {
  list(expr, silent, outfile)
  function(cont, ..., ret, wind) {
    outfile_ <- NULL
    silent_ <- NULL
    wind_try <- function(to_try, ...) {
      trace(where <- "winding up")
      returning <- tryCatch(to_try(...), error=function(e) {
        trace(where <- "try caught")
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
        trace(where <- "try formatted err msg")
        .Internal(seterrmessage(msg[1L]))
        if (!silent_ && isTRUE(getOption("show.error.messages"))) {
          cat(msg, file = outFile_)
          .Internal(printDeferredWarnings())
        }
        retval <- invisible(structure(msg, class = "try-error", condition = e))
        # }}} (copied)
        ret(cont, retval)
      })
    }

    # provide "wind" context argument, which arg_cps will use
    getExpr <- expr(cont, ..., ret=ret, wind=wind_try)
    gotSilent <- function(val) {silent_ <<- val; getExpr()}
    getSilent <- silent(gotSilent, ..., ret=ret, wind=wind)
    gotOutfile <- function(val) {outfile_ <<- val; getSilent()}
    getOutfile <- outfile(gotOutfile, ..., ret=ret, wind=wind)
  }
}
