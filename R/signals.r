# arg_cps interfaces between normal R evaluation and the CPS interpreter.
#
# It saves its arg, unevaluated, and feeds it to the continuation function.
arg_cps <- function(x) {
  x <- arg(x)

  function(cont, ..., stop=base::stop) {
    list(cont, stop)
    function() {
      trace("arg: ", deparse(expr(x)))
      do(cont, x)
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
  if (length(list(...)) > 0) stop("Unsupported arguments to tryCatch_cps")
  if (is_missing(finally)) {
    if (is_missing(error)) {
      function(cont, ...) expr(cont, ...)
    } else {
      catch_ctor(expr, error)
    }
  } else {
    if (is_missing(error)) {
      finally_ctor(expr, finally)
    } else {
      finally_ctor(catch_ctor(expr, error), finally)
    }
  }
}

catch_ctor <- function(expr, error) {
  list(expr, error)
  function(cont, ..., ret, stop, brk, nxt, windup, unwind, return) {
    list(cont, ret, stop, maybe(brk), maybe(nxt), windup, unwind, return)
    # Remember, flow of the handlers goes from bottom to top
    result <- NULL
    gotErrHandler <- function(val) {
      trace("try-catch got error handler")
      if (is.function(val)) {
        trace("try-catch calling handler")
        val <- val(result)
      }
      ret(cont, val)
    }
    getErrHandler <- error(gotErrHandler, ..., ret=ret, stop=stop,
                           brk=brk, nxt=nxt, windup=windup,
                           unwind=unwind, return=return)
    stop_ <- function(err) {
      trace("try-catch stop")
      result <<- err
      unwind(getErrHandler)
    }
    return_ <- function(val) {
      trace("try-catch return")
      unwind(return, val)
    }
    brk_ <- function() {
      trace("try-catch break")
      unwind(brk)
    }
    nxt_ <- function() {
      trace("try-catch nxt")
      unwind(nxt)
    }
    after <- function(val) {
      trace("try-catch success?")
      force(val) #need to to prevent us lazily
                 #propagating an error past the unwind
      unwind(cont, val)
    }
    do_expr <- expr(after, ...,
                    ret=ret, stop=stop_, brk=brk_, nxt=nxt_,
                    windup=windup, unwind=unwind, return=return_)
    do_windup <- function(f, ...) {
      list(f, ...)
      trace("try-catch windup")
      on.exit(trace("try-catch unwind"))
      tryCatch(f(...), error=function(e) {
        trace("try-catch caught error by windup")
        stop_(e)
      })
    }
    windup_ <- function() {
      result <<- NULL
      windup(do_windup, do_expr)
    }
    windup_
  }
}

finally_ctor <- function(expr, finally) {
  list(expr, finally)
  function(cont, ..., ret, stop, brk, nxt, windup, unwind, return) {
    list(cont, ..., ret, stop, maybe(brk), maybe(nxt), windup, unwind, return)
    # Deep breath. Remember, the handlers flow from bottom to top!
    result <- NULL
    after <- NULL
    after_finally <- function(val) {
      force(val)
      trace("try-finally after")
      switch(after,
             caught=,
             success=cont(result),
             failure=stop(result),
             return=return(result),
             `next`=nxt(),
             `break`=brk())
    }
    # if there is an uncaught error, then a return, break or next
    # from the finally block, throw the saved error instead.
    finally_return <- function(...) if (after=="failure") stop(result) else return(...) 
    finally_brk <- function() if (after=="failure") stop(result) else brk()
    finally_nxt <- function() if (after=="failure") stop(result) else nxt()
    do_finally <- finally(after_finally, ...,
                            ret=ret, stop=stop, brk=finally_brk, nxt=finally_nxt,
                            windup=windup, unwind=unwind, return=finally_return)
    finally_then <- function(val) {
      result <<- val
      after <<- "success"
      do_finally() # val should be saved, discard it
    }
    stop_ <- function(err) {
      trace("try-finally stop")
      result <<- err
      after <<- "failure"
      unwind(do_finally)
    }
    return_ <- function(val) {
      trace("try-finally return")
      result <<- val
      after <<- "return"
      unwind(do_finally)
    }
    brk_ <- function() {
      trace("try-finally break")
      after <<- "break"
      unwind(do_finally)
    }
    nxt_ <- function() {
      trace("try-finally next")
      after <<- "next"
      unwind(do_finally)
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
      result <<- NULL
      windup(do_windup, do_expr)
    }
    windup_
  }
}


try_cps <- function(expr, silent=arg_cps(FALSE),
                    outfile=arg_cps(getOption("try.outFile", default = stderr()))) {
  list(expr, silent, outfile)
  function(cont, ..., ret) {

    outfile_ <- NULL
    silent_ <- NULL

    tc <- tryCatch_cps(expr, error=arg_cps(function(err) {
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
    }))(cont, ..., ret=ret)

    gotSilent <- function(val) {silent_ <<- val; tc()}
    getSilent <- silent(gotSilent, ..., ret=ret)
    gotOutfile <- function(val) {outfile_ <<- val; getSilent()}
    getOutfile <- outfile(gotOutfile, ..., ret=ret)
  }
}
