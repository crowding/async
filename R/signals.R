return_cps <- function(.contextName, x) {
  force(.contextName)
  maybe(x)
  return_ <- function(cont, ..., rtn, trace=trace_) {
    list(cont, rtn, trace)
    if (missing_(arg(x))) {
      node(return_ <- function() rtn(NULL))
      # this is our "rtn" callback not base::return
    } else {
      x(rtn, ..., rtn=rtn, trace=trace)
    }
  }
}

tryCatch_cps <- function(.contextName, expr, ..., error, finally) {
  list(.contextName, expr, maybe(error), maybe(finally))
  assert(length(list(...)) == 0, "Unsupported arguments to tryCatch_cps")
  if (missing_(arg(finally))) {
    if (missing_(arg(error))) {
      expr
    } else {
      catch_cps_(.contextName, expr, error)
    }
  } else {
    if (missing_(arg(error))) {
      finally_cps_(.contextName,
                   expr,
                   finally)
    } else {
      finally_cps_(.contextName,
                   catch_cps_(paste0(.contextName, ".catch"), expr, error),
                   finally)
    }
  }
}

catch_cps_ <- function(.contextName, expr, error) {
  list(.contextName, expr, error)
  function(cont, ..., stp, brk, nxt, goto, windup, unwind,
           rtn, trace=trace_) {
    list(cont, stp, maybe(brk), maybe(nxt), maybe(goto), windup, unwind, rtn,
         trace)
    # Remember, flow of the handlers goes from bottom to top
    result <- NULL
    node(gotErrHandler <- function(val) {
      if (is.function(val)) {
        trace("catch: calling error handler\n")
        val <- val(result)
      }
      cont(val)
    })
    getErrHandler <- error(gotErrHandler, ..., stp=stp, brk=brk,
                           goto=goto, nxt=nxt, windup=windup, unwind=unwind,
                           rtn=rtn, trace=trace)
    node(do_return <- function() {
      rtn(list(result, result <<- NULL)[[1]])
    })
    node(do_continue <- function() {
      cont(list(result, result <<- NULL)[[1]])
    })
    node(continue <- function(val) {
      trace("catch: continue\n")
      result <<- val
      unwind(do_continue)
    })
    node(return_ <- function(val) {
      trace("catch: return\n")
      result <<- val
      unwind(do_return)
    })
    node(stop_ <- function(val) {
      trace("catch: stop\n")
      result <<- val
      unwind(getErrHandler)
    })
    if(!is_missing(brk)) {
      node(brk_ <- function() {
        trace("catch: break\n")
        unwind(brk)
      })
    } else brk_ <- missing_value()
    if(!is_missing(nxt)) {
      node(nxt_ <- function() {
        trace("catch: next\n")
        unwind(nxt)
      })
    } else nxt_ <- missing_value()
    if (!is_missing(goto)) {
      node(doGoto <- function() {
        goto(result)
      })
      node(goto_ <- function(val) {
        trace("catch: goto\n")
        result <<- val
        unwind(doGoto)
      })
    } else goto_ <- missing_value()
    do_expr <- expr(continue, ...,
                    stp=stop_, brk=brk_, nxt=nxt_, goto=goto_,
                    windup=windup, unwind=unwind,
                    rtn=return_, trace=trace)
    node(do_windup <- function(cont, ...) {
      list(cont, ...)
      trace("catch: windup\n")
      on.exit({trace("catch: unwind\n"); NULL})
      tryCatch(cont(...), error=function(e) {
        trace("catch: catch in windup\n")
        stop_(e) # a tailcall in a lambda, "counts" as a tailcall from do_windup
      })
    })
    node(try_ <- function() {
      result <<- NULL
      trace("catch: begin\n")
      windup(do_expr, do_windup)
    })
    try_
  }
}

finally_cps_ <- function(.contextName, expr, finally) {
  list(.contextName, expr, finally)
  function(cont, ..., stp, brk, nxt, goto, windup, unwind, rtn, trace=trace_) {
    list(cont, stp, maybe(brk), maybe(nxt), maybe(goto),
         windup, unwind, rtn, trace)
    # Deep breath. Remember, the handlers flow from bottom to top!
    result <- NULL
    after <- NULL

    #this bquoting stuff is to keep the compiler from following
    #nonexistent path to brk/nxt/etc.
    node(continue <- eval(bquote(splice=TRUE, function(val) {
      force(val)
      trace(paste0("finally: continue??? with ", after, "\n"))
      # {{ }} prevents covr tracing
      {{switch(after,
             success=cont(result),
             stop=stp(result),
             return=rtn(result),
             ..(c(list(),
                  if (!is_missing(nxt)) alist(`next`={
                    nxt()
                  }),
                  if (!is_missing(brk)) alist(`break`={
                    brk()
                  }),
                  if (!is_missing(goto)) alist(goto={
                    goto(result)
                  })
             )),
             stp(simpleError(paste0("Unexpected after-finally action: ",
                             as.character(after))))
             )}}
    })))
    # if there is an uncaught saved error, then a jump out of
    # the finally block, throw the saved error instead of jumping.
    node(finally_return <- function(val) {
      trace("finally: return in finally block\n")
      if (after=="stop") stp(result) else rtn(val)
    })
    if(!is_missing(brk)) {
      node(finally_brk <- function() {
        trace("finally: break in finally block\n")
        if (after=="failure") stp(result) else brk()
      })
    } else finally_brk <- missing_value()
    if(!is_missing(nxt)) {
      node(finally_nxt <- function() {
        trace("finally: next in finally block\n")
        if (after=="failure") stp(result) else nxt()
      })
    } else finally_nxt_ <- missing_value()
    if(!is_missing(goto)) {
      node(finally_goto <- function(val) {
        trace("finally: goto in finally block")
        if (after=="failure") stp(result) else goto(val)
      })
    } else finally_goto_ <- missing_value()
    do_finally <- finally(continue, ...,
                          stp=stp, brk=finally_brk, nxt=finally_nxt,
                          goto=finally_goto,
                          windup=windup, unwind=unwind, rtn=finally_return,
                          trace=trace)
    node(finally_then <- function(val) {
      result <<- val
      after <<- "success"
      trace("finally: success\n")
      unwind(do_finally) # val should be saved, discard it
    })
    node(stop_ <- function(err) {
      trace("finally: stop\n")
      result <<- err
      after <<- "stop"
      unwind(do_finally)
    })
    node(return_ <- function(val) {
      trace("finally: return\n")
      result <<- val
      after <<- "return"
      unwind(do_finally)
    })
    if (!is_missing(brk)) {
      node(brk_ <- function() {
        trace("finally: break\n")
        after <<- "break"
        unwind(do_finally)
      })
    } else brk_ <- missing_value()
    if(!is_missing(nxt)) {
      node(nxt_ <- function() {
        trace("finally: next\n")
        after <<- "next"
        unwind(do_finally)
      })
     } else nxt_ <- missing_value()
    if(!is_missing(goto)) {
      node(goto_ <- function(val) {
        result <<- val
        after <<- "goto"
        unwind(do_finally)
      })
    } else goto_ <- missing_value()
    do_expr <- expr(finally_then, ..., stp=stop_, brk=brk_, nxt=nxt_, goto=goto_,
                    windup=windup, unwind=unwind, rtn=return_,
                    trace=trace)
    node(do_windup <- function(cont) {
      list(cont)
      trace("finally: windup\n")
      on.exit({trace("finally: unwind\n"); NULL})
      tryCatch(cont(), error=function(err) {
        trace("finally: catch in windup\n")
        stop_(err)
      })
    })
    node(try_ <- function() {
      after <<- NULL
      result <<- NULL
      trace("finally: begin\n")
      windup(do_expr, do_windup)
    })
    try_
  }
}

try_handler <- function(val, silent_, outfile_) {
  # nocov start
  # (copied from base::try) {{{
  call <- conditionCall(val)
  if (!is.null(call)) {
    if (identical(call[[1L]], as.name("doTryCatch")))
      call <- sys.call(-4L)
    dcall <- deparse(call)[1L]
    prefix <- paste("Error in", dcall, ": ")
    LONG <- 75L
    sm <- strsplit(conditionMessage(val), "\n")[[1L]]
    w <- 14L + nchar(dcall, type = "w") + nchar(sm[1L],
                                                type = "w")
    if (is.na(w))
      w <- 14L + nchar(dcall, type = "b") + nchar(sm[1L],
                                                  type = "b")
    if (w > LONG)
      prefix <- paste0(prefix, "\n  ")
  }
  else prefix <- "Error : "
  msg <- paste0(prefix, conditionMessage(val), "\n")
  if (!silent_ && isTRUE(getOption("show.error.messages"))) {
    cat(msg, file = outfile_)
  }
  invisible(structure(msg, class = "try-error", condition = val))
  # }}} (copied)
  # nocov end
}

try_cps <- function(.contextName, expr,
                    silent=R(paste0(.contextName, ".silent"), FALSE),
                    outfile=R(paste0(.contextName, ".outfile"),
                              getOption("try.outFile", default = stderr()))) {
  list(.contextName, expr, silent, outfile)
  function(cont, ..., trace=trace_) {
    list(cont, trace)
    outfile_ <- NULL
    silent_ <- NULL

    returnHandler <- NULL
    node(getHandler <- function() {
      val <- function(val) {x <- try_handler(val, silent_, outfile_); x}
      returnHandler(val)
    })

    tryCatch_ctor <- tryCatch_cps(
      paste0(.contextName, ".tryCatch"),
      expr,
      error=function(cont, ...) {
        returnHandler <<- cont
        getHandler
      })
    doTryCatch <- tryCatch_ctor(cont, ..., trace=trace)

    node(gotSilent <- function(val) {silent_ <<- val; doTryCatch()})
    getSilent <- silent(gotSilent, ..., trace=trace)
    node(gotOutfile <- function(val) {outfile_ <<- val; getSilent()})
    getOutfile <- outfile(gotOutfile, ..., trace=trace)
    getOutfile
  }
}

