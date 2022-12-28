# R() wraps an R argument into an execution node
R <- function(x) {
  x <- arg(x)

  function(cont, ..., stop=base::stop, trace=trace_) {
    list_missing(cont, stop)
    x <- x

    R_ <- function() {
      trace(paste0("R: ", deparse(nseval::expr(x)), "\n"))
      val <- NULL
      nseval::set_arg(val, x)
      cont(val)
    }
  }
}

is_R <- function(f) {
  exists("R_", environment(f), inherits=FALSE) &&
    identical(f, get("R_", environment(f)))
}

R_expr <- function(f) {
  expr(get("x", environment(f)))
}

R_env <- function(f) {
  env(get("x", environment(f)))
}

return_cps <- function(x) {
  maybe(x)
  return_ <- function(cont, ..., return, trace=trace_) {
    list(cont, return, trace)
    if (missing_(arg(x))) {
      return_ <- function() return(NULL)
      # this is our "return" callback not base::return
    } else {
      x(return, ..., return=return, trace=trace)
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
  function(cont, ..., stop, brk, nxt, windup, unwind,
           return, trace=trace_) {
    list(cont, stop, maybe(brk), maybe(nxt), windup, unwind, return,
         trace)
    # Remember, flow of the handlers goes from bottom to top
    result <- NULL
    gotErrHandler <- function(val) {
      if (is.function(val)) {
        trace("catch: calling error handler\n")
        val <- val(result)
      }
      cont(val)
    }
    getErrHandler <- error(gotErrHandler, ..., stop=stop, brk=brk,
                           nxt=nxt, windup=windup, unwind=unwind,
                           return=return, trace=trace)
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
    do_expr <- expr(after, ..., stop=stop_, brk=brk_, nxt=nxt_,
                    windup=windup, unwind=unwind, return=return_,
                    trace=trace)
    do_windup <- function(cont, ...) {
      list(cont, ...)
      trace("catch: windup\n")
      on.exit(trace("catch: unwind\n"))
      tryCatch(cont(...), error=function(e) {
        trace("catch: catch in windup\n")
        stop_(e)
      })
      NULL
    }
    try_ <- function() {
      result <<- NULL
      trace("catch: begin\n")
      windup(do_expr, do_windup)
    }
    try_
  }
}

finally_cps_ <- function(expr, finally) {
  list(expr, finally)
  function(cont, ..., stop, brk, nxt, windup, unwind, return, trace=trace_) {
    list(cont, stop, maybe(brk), maybe(nxt), windup, unwind, return, trace)
    # Deep breath. Remember, the handlers flow from bottom to top!
    result <- NULL
    after <- NULL

    if (missing(nxt)) {
      #this is to keep the compiler from following a nonexistent path to brk/nxt
      continue <- function(val) {
        force(val)
        trace(paste0("finally: continue with ", after, "\n"))
        switch(after,
               success=cont(result),
               stop=stop(result),
               return=return(result),
               stop("Unexpected final action"))
      }
    } else {
      continue <- function(val) {
        force(val)
        trace(paste0("finally: continue with ", after, "\n"))
        switch(after,
               success=cont(result),
               stop=stop(result),
               return=return(result),
               `next`=nxt(),
               `break`=brk(),
               stop("Unexpected final action"))
      }
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
    do_finally <- finally(continue, ...,
                          stop=stop, brk=finally_brk, nxt=finally_nxt,
                          windup=windup, unwind=unwind, return=finally_return,
                          trace=trace)
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
    do_expr <- expr(finally_then, ..., stop=stop_, brk=brk_, nxt=nxt_,
                    windup=windup, unwind=unwind, return=return_,
                    trace=trace)
    do_windup <- function(cont, ...) {
      list(cont, ...)
      trace("finally: windup\n")
      on.exit(trace("finally: unwind\n"))
      tryCatch(cont(...), error=function(err) {
        trace("finally: catch in windup\n")
        stop_(err)
      })
      NULL
    }
    try_ <- function() {
      after <<- NULL
      result <<- NULL
      trace("finally: begin\n")
      windup(do_expr, do_windup)
    }
    try_
  }
}

try_cps <- function(expr, silent=R(FALSE),
                       outfile=R(getOption("try.outFile", default = stderr()))) {
  list(expr, silent, outfile)
  function(cont, ..., stop, brk, nxt, windup, unwind, return, trace=trace_) {
    list(cont, stop, maybe(brk), maybe(nxt), windup, unwind, return, trace)
    # Remember, flow of the handlers goes from bottom to top
    result <- NULL
    outfile_ <- NULL
    silent_ <- NULL

    stop_ <- function(err) {
      trace("try: stop\n")
      result <<- err
      unwind(try_handler)
    }
    return_ <- function(val) {
      trace("try: return\n")
      unwind(return, val)
    }
    brk_ <- function() {
      trace("try: break\n")
      unwind(brk)
    }
    nxt_ <- function() {
      trace("try: next\n")
      unwind(nxt)
    }
    after <- function(val) {
      force(val) #need to to prevent us lazily
      #propagating an error past the unwind
      trace("try: success\n")
      unwind(cont, val)
    }
    try_handler <- function() {
      # (copied from base::try) {{{
      trace("try: handler\n")
      call <- conditionCall(result)
      if (!is.null(call)) {
        if (identical(call[[1L]], as.name("doTryCatch")))
          call <- sys.call(-4L)
        dcall <- deparse(call)[1L]
        prefix <- paste("Error in", dcall, ": ")
        LONG <- 75L
        sm <- strsplit(conditionMessage(result), "\n")[[1L]]
        w <- 14L + nchar(dcall, type = "w") + nchar(sm[1L],
                                                    type = "w")
        if (is.na(w))
          w <- 14L + nchar(dcall, type = "b") + nchar(sm[1L],
                                                      type = "b")
        if (w > LONG)
          prefix <- paste0(prefix, "\n  ")
      }
      else prefix <- "Error : "
      msg <- paste0(prefix, conditionMessage(result), "\n")
      if (!silent_ && isTRUE(getOption("show.error.messages"))) {
        cat(msg, file = outfile_)
      }
      retval <- invisible(structure(msg, class = "try-error", condition = result))
      # }}} (copied)
      cont(retval)
    }
    do_expr <- expr(after, ...,
                    stop=stop_, brk=brk_, nxt=nxt_,
                    windup=windup, unwind=unwind, return=return_, trace=trace)
    do_windup <- function(cont, ...) {
      list(cont, ...)
      trace("try: windup\n")
      on.exit(trace("try: unwind\n"))
      tryCatch(cont(...), error=function(e) {
        trace("try: caught error by windup\n")
        stop_(e)
      })
      NULL
    }
    try_ <- function() {
      result <<- NULL
      trace("try: begin\n")
      windup(do_expr, do_windup)
    }
    gotSilent <- function(val) {silent_ <<- val; try_()}
    getSilent <- silent(gotSilent, ..., stop=stop_, brk=brk_, nxt=nxt_,
                    windup=windup, unwind=unwind, return=return_, trace=trace)
    gotOutfile <- function(val) {outfile_ <<- val; getSilent()}
    getOutfile <- outfile(gotOutfile, ..., stop=stop_, brk=brk_, nxt=nxt_,
                    windup=windup, unwind=unwind, return=return_, trace=trace)
    getOutfile
  }
}
