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
      function() return(NULL) # this is our "return" callback not base::return
    } else {
      x(return, ..., ret=ret, return=return)
    }
  }
}

tryCatch_cps <- function(expr, ..., error, finally) {
  list(expr, maybe(error), maybe(finally))
  function(cont, ..., ret, stop, brk, nxt, windup, unwind, return) {
    list(cont, ..., ret, stop, maybe(brk), maybe(nxt), windup, unwind, return)
    if (is_missing(error)) {
      if (is_missing(finally)) {
        #no-op
        cont(cont, ..., ret=ret, stop=stop, brk=brk, nxt=nxt,
             windup=windup, unwind=unwind, return=return)
      } else {
        # try-finally
        value <- NULL
        error <- NULL
        after <- NULL
        after_finally <- function(val) {
          switch(after,
                 success=cont(value),
                 failure=stop(error),
                 return=return(value),
                 `next`=nxt(),
                 `break`=brk())
        }
        # if there is an uncaught error, then a return, break or next
        # from the finally block, throw the saved error instead.
        finally_return <- function(...) if (after=="failure") stop(error) else return(...) 
        finally_brk <- function() if (after=="failure") stop(error) else brk()
        finally_nxt <- function() if (after=="failure") stop(error) else nxt()
        finally_then <- finally(after_finally, ...,
                                ret=ret, stop=stop, brk=finally_brk, nxt=finally_nxt,
                                windup=windup, unwind=unwind, return=finally_return)
        stop_ <- function(err) {
          trace("try-finally stop")
          error <<- err
          after <<- "failure"
          unwind()
          ret(finally_then)
        }
        return_ <- function(val) {
          trace("try-finally return")
          value <<- val
          after <<- "return"
          unwind(finally_then)
        }
        brk_ <- function() {
          trace("try-finally break")
          after <<- "break"
          unwind(finally_then)
        }
        nxt_ <- function() {
          trace("try-finally next")
          after <<- "next"
          unwind(finally_then)
        }
        do_expr <- expr(finally_then, ...,
                        ret=ret, stop=stop_, brk=brk_, nxt=nxt_,
                        windup=windup, unwind=unwind, return=return_)
        do_windup <- function(f, ...) {
          list(f, ...)
          trace("try-finally winding up")
          on.exit(trace("try-finally unwinding"))
          tryCatch(f(...), error=stop_)
        }
        windup_ <- function() {
          after <<- NULL
          value <<- NULL
          error <<- NULL
          windup(do_windup, do_expr)
        }
        windup_
      }
    } else {
      stop("catch unimplemented")
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
