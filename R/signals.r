# R() wraps an R argument into an execution node
R <- function(x) {
  x <- arg(x)

  function(cont, ..., stop=base::stop, trace=trace_) {
    list_missing(cont, stop)
    x <- x
    #give it a name so that the graph drawer can detect it
    R_ <- function(...) {
      trace(paste0("R: ", deparse(expr(x)), "\n"))
      #there's presently a slight issue with nseval here when x stops
      #do(cont, x)
      #but this works ok...
      set_dots(environment(), x)
      cont(...)
    }
  }
}

return_cps <- function(x) {
  maybe(x)
  function(cont, ..., ret, return, trace=trace_) {
    list(cont, ret, return, trace)
    if (missing_(arg(x))) {
      return_ <- function() return(NULL) # this is our "return" callback not base::return
    } else {
      x(return, ..., ret=ret, return=return, trace=trace)
    }
  }
}

tryCatch_cps <- function(expr, ..., error, finally) {
  assert(length(list(...)) == 0, "Unsupported arguments to tryCatch_cps")
  if (missing_(arg(finally))) {
    if (missing_(arg(error))) {
      expr
    } else {
      catch_cps_(expr, error)
    }
  } else {
    if (missing_(arg(error))) {
      finally_cps_(expr, finally)
    } else {
      finally_cps_(catch_cps_(expr, error), finally)
    }
  }
}

catch_cps_ <- function(expr, error) {
  list(expr, error)
  function(cont, ..., ret, stop, brk, nxt, windup, unwind, return, trace=trace_) {
    list(cont, ret, stop, maybe(brk), maybe(nxt), windup, unwind, return, trace)
    # Remember, flow of the handlers goes from bottom to top
    result <- NULL
    gotErrHandler <- function(val) {
      if (is.function(val)) {
        trace("catch: calling error handler\n")
        val <- val(result)
      }
      ret(cont, val)
    }
    getErrHandler <- error(gotErrHandler, ..., ret=ret, stop=stop,
                           brk=brk, nxt=nxt, windup=windup,
                           unwind=unwind, return=return, trace=trace)
    stop_ <- function(err) {
      trace("catch: stop\n")
      result <<- err
      unwind(getErrHandler)
    }
    return_ <- function(val) {
      trace("catch: return\n")
      unwind(return, val)
    }
    brk_ <- function() {
      trace("catch: break\n")
      unwind(brk)
    }
    nxt_ <- function() {
      trace("catch: next\n")
      unwind(nxt)
    }
    after <- function(val) {
      force(val) #need to to prevent us lazily
      #propagating an error past the unwind
      trace("catch: success\n")
      unwind(cont, val)
    }
    do_expr <- expr(after, ...,
                    ret=ret, stop=stop_, brk=brk_, nxt=nxt_,
                    windup=windup, unwind=unwind, return=return_, trace=trace)
    do_windup <- function(f, ...) {
      list(f, ...)
      if (verbose) trace("catch: windup\n")
      on.exit(if (verbose) trace("catch: unwind\n"))
      tryCatch(f(...), error=function(e) {
        trace("catch: catch in windup\n")
        stop_(e)
      })
    }
    windup_ <- function() {
      result <<- NULL
      trace("catch: begin\n")
      windup(do_windup, do_expr)
    }
    windup_
  }
}

finally_cps_ <- function(expr, finally) {
  list(expr, finally)
  function(cont, ..., ret, stop, brk, nxt, windup, unwind, return, trace=trace_) {
    list(cont, ret, stop, maybe(brk), maybe(nxt), windup, unwind, return, trace)
    # Deep breath. Remember, the handlers flow from bottom to top!
    result <- NULL
    after <- NULL
    after_finally <- function(val) {
      force(val)
      trace(paste0("finally: continue with ", after, "\n"))
      switch(after,
             success=cont(result),
             stop=stop(result),
             return=return(result),
             `next`=nxt(),
             `break`=brk())
    }
    # if there is an uncaught error, then a return, break or next
    # from the finally block, throw the saved error instead.
    finally_return <- function(...) {
      trace("finally: return in finally block\n")
      if (after=="stop") stop(result) else return(...)
    }
    finally_brk <- function() {
      trace("finally: break in finally block\n")
      if (after=="failure") stop(result) else brk()
    }
    finally_nxt <- function() {
      trace("finally: next in finally block\n")
      if (after=="failure") stop(result) else nxt()
    }
    do_finally <- finally(after_finally, ...,
                          ret=ret, stop=stop, brk=finally_brk, nxt=finally_nxt,
                          windup=windup, unwind=unwind, return=finally_return, trace=trace)
    finally_then <- function(val) {
      result <<- val
      after <<- "success"
      trace("finally: success\n")
      do_finally() # val should be saved, discard it
    }
    stop_ <- function(err) {
      trace("finally: stop\n")
      result <<- err
      after <<- "stop"
      unwind(do_finally)
    }
    return_ <- function(val) {
      trace("finally: return\n")
      result <<- val
      after <<- "return"
      unwind(do_finally)
    }
    brk_ <- function() {
      trace("finally: break\n")
      after <<- "break"
      unwind(do_finally)
    }
    nxt_ <- function() {
      trace("finally: next\n")
      after <<- "next"
      unwind(do_finally)
    }
    do_expr <- expr(finally_then, ...,
                    ret=ret, stop=stop_, brk=brk_, nxt=nxt_,
                    windup=windup, unwind=unwind, return=return_, trace=trace)
    do_windup <- function(f, ...) {
      list(f, ...)
      if (verbose) trace("finally: windup\n")
      on.exit(if(verbose) trace("finally: unwind\n"))
      tryCatch(f(...), error=function(err) {
        trace("finally: catch in windup\n")
        stop_(err)
      })
    }
    windup_ <- function() {
      after <<- NULL
      result <<- NULL
      trace("finally: begin\n")
      windup(do_windup, do_expr)
    }
    windup_
  }
}

try_cps <- function(expr, silent=R(FALSE),
                    outfile=R(getOption("try.outFile", default = stderr()))) {
  list(expr, silent, outfile)
  function(cont, ..., ret) {

    outfile_ <- NULL
    silent_ <- NULL

    try_handler <- function(err) {
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
      if (!silent_ && isTRUE(getOption("show.error.messages"))) {
        cat(msg, file = outfile_)
      }
      retval <- invisible(structure(msg, class = "try-error", condition = err))
      # }}} (copied)
      ret(cont, retval)
    }

    tc <- tryCatch_cps(expr, error=R(try_handler))(cont, ..., ret=ret)

    gotSilent <- function(val) {silent_ <<- val; tc()}
    getSilent <- silent(gotSilent, ..., ret=ret)
    gotOutfile <- function(val) {outfile_ <<- val; getSilent()}
    getOutfile <- outfile(gotOutfile, ..., ret=ret)
  }
}
