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
  function(cont, ..., ret, stop) {
    outfile_ <- NULL
    silent_ <- NULL

    stop_ <- function(err) {
      trace(where <- "try caught")
      # (copied from base::try) {{{
      call <- conditionCall(err)
      if (!is.null(call)) {
        if (identical(call[[1L]], quote(doTryCatch)))
          call <- sys.call(-4L)
        dcall <- deparse(call)[1L]
        prefix <- paste("Error in", dcall, ": ")
        LONG <- 75L
        sm <- strsplit(conditionMessage(err), "\n")[[1L]]
        w <- 14L + nchar(dcall, type = "w") + nchar(sm[1L],
                                                    type = "w")
        if (is.na(w))
          w <- 14L + nchar(dcall, type = "b") + nchar(sm[1L],
                                                      type = "b")
        if (w > LONG)
          prefix <- paste0(prefix, "\n  ")
      }
      else prefix <- "Error : "
      msg <- paste0(prefix, conditionMessage(err), "\n")
      trace(where <- "try formatted err msg")
      .Internal(seterrmessage(msg[1L]))
      if (!silent_ && isTRUE(getOption("show.error.messages"))) {
        cat(msg, file = outFile_)
        .Internal(printDeferredWarnings())
      }
      retval <- invisible(structure(msg, class = "try-error", condition = err))
      # }}} (copied)
      ret(cont, retval)
    }

    getExpr <- expr(cont, ..., ret=ret, stop=stop_)
    gotSilent <- function(val) {silent_ <<- val; getExpr()}
    getSilent <- silent(gotSilent, ..., ret=ret, stop=stop)
    gotOutfile <- function(val) {outfile_ <<- val; getSilent()}
    getOutfile <- outfile(gotOutfile, ..., ret=ret, stop=stop)
  }
}
