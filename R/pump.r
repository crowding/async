# pump.r implements the _base level_ loop common to all coroutines

assert <- function(condition,
                   msg=c(
                     "assertion failed: ",
                     deparse(src),
                     if(!is.null(getSrcref(src))) c(
                       "at ", getSrcDirectory(src), "/", getSrcFilename(src),
                       ":", getSrcLocation(src, "line")))) {
  if (!isTRUE(condition)) {
    src <- arg_expr(condition)
    stop(msg)
  }
}

#' @param prefix Character prefix to print before the trace.
#' @rdname async
#' @export
with_prefix <- function(prefix) function(...)
  cat(paste0(prefix, ": ", ...), sep="")

reset <- function(...) list(...)[[1]]

add_class <- function(x, ...) {
  attr(x, "class") <- c(..., attr(x, "class"))
  x
}

pump <- function(expr, stp=stop, ...) {
  thisPump <- make_pump(expr, stp=stp, ...)
  thisPump()
}

make_pump <- function(expr, ...,
                      bounce=stop("unused"),
                      windup=stop("unused"),
                      unwind=stop("unused"),
                      pause=stop("unused"),
                      goto=stop("unused"),
                      evl=stop("unused"),
                      sto=stop("unused"),
                      stp=structure(\(x)function(x){stop(x); NULL},
                                    globalName="stp", localName="stp"),
                      rtn=structure(function(x)x, localName="rtn"),
                      trace=trace_,
                      catch=TRUE,
                      targetEnv) {

  .contextName <- "pump"
  list(expr, stp, rtn, trace, targetEnv)
  nonce <- (function() NULL)

  action <- "pause" # stopped, pause
  pumpCont <- nonce
  value <- nonce
  debugR <- FALSE
  debugInternal <- FALSE

  setDebug <- structure(function(R, internal) {
    if (!missing(R)) debugR <<- R
    if (!missing(internal)) debugInternal <<- internal
    x <- list(R=debugR, internal=debugInternal)
    x
  }, localName="setDebug", globalName="setDebug")

  getCont %<g-% structure(function() {
    # For display, return a string describing the current state.
    g <- attr(pumpCont, "globalName")
    if (is.null(g)) {
      paste0(get0(".contextName", environment(pumpCont), ifnotfound="???"),
             "__", attr(pumpCont, "localName"))
    } else g
  }, localName="getCont", globalName="getCont")

  pause_ %<-% function(cont) {
    trace("pump: pause (awaiting)\n")
    pumpCont <<- cont
    action <<- "pause"
  }

  pause_val_ %<-% function(cont, val) {
    trace("pump: pause (yielding)\n")
    pumpCont <<- cont
    value <<- val
    action <<- "pause_val"
  }

  bounce_ %<-% function(cont) {
    trace("pump: bounce\n")
    pumpCont <<- cont
    action <<- "continue"
  }

  bounce_val_ %<-% function(cont, val) {
    force(val)
    trace("pump: bounce with value\n")
    value <<- val
    pumpCont <<- cont
    action <<- "continue_val"
  }

  evl_ %<-% function(cont, val) {
    if(debugR)
      val <- eval(substitute({browser(); x}, list(x=val)), targetEnv)
    else
      val <- eval(val, targetEnv)
    cont(val)
  }

  sto_ %<-% function(cont, where, val) {
    targetEnv[[where]] <- val
    cont()
  }

  stop_ %<g-% function(val) {
    trace(paste0("pump: stop: ", conditionMessage(val), "\n"))
    value <<- val
    after_exit <<- "stop"
    action <<- "exit"
    pumpCont <<- function() doExits()
  }

  return_ %<g-% function(val) {
    trace("pump: return\n")
    force(val)
    value <<- val
    after_exit <<- "finish"
    doExits()
  }

  # We maintain a list of "windings."
  # A "winding" is a function that must tailcall into its "cont" arg. like:
  # null_winding <- function(cont) tryCatch(cont())
  # f(cont) that establishes a context,
  # and returning from f(cont), unwinds that context.
  if(catch) {
    base_winding %<-% function(cont) {
      trace("pump: windup\n")
      repeat tryCatch({tmp <- cont(); return(tmp)}, error=function(err){
        trace(paste0("pump: caught error by windup: ",
                     conditionMessage(err), "\n"))
        if (action %in% c("finish", "stop"))
          stp(value)
        else stop_(err)
      }, finally=trace("pump: unwind\n"))
    }
  } else {
    base_winding <- function(cont) cont()
  }
  windings <- list(base_winding) # <- FIXME this will leak scope!!!!

  # this needs special handling in walk() because there are TWO
  # function pointers and both need to be treated as nodes?
  windup_ %<-% function(cont, winding) {
    list(cont, winding)
    trace("pump: Adding to windup list\n")
    outerWinding <- windings[[1]]
    g <- function(cont) {
      # Call outer winding first and have it continue to the
      # new winding, which continues to 'cont'
      outerWinding(function() winding(cont))
    }
    windings <<- c(list(g), windings)
    pumpCont <<- cont
    action <<- "rewind"
  }

  unwind_ %<-% function(cont) {
    trace("pump: removing from windup list\n")
    windings[[1]] <<- NULL
    pumpCont <<- cont
    action <<- "rewind"
  }

  doWindup %<-% structure(function(cont) {
    windings[[1]](cont)
  }, localName="doWindup", globalName="doWindup")

  exit_list <- list()
  after_exit <- "xxx"

  addExit %<-% function(cont, handle, add, after) {
    if (add) {
      if (after) {
        exit_list <<- c(exit_list, list(handle))
      } else {
        exit_list <<- c(list(handle), exit_list)
      }
    } else {
      exit_list <<- list(handle)
    }
    cont(invisible(NULL))
  }

  using_onexit <- FALSE
  exit_ctors <- list()

  registerExit %<-% function(exit) {
    using_onexit <<- TRUE
    handle <- length(exit_ctors) + 1
    name <- paste0("exit_", as.character(handle))
    # hold off on constructing them
    exit_ctors <<- c(exit_ctors, structure(list(exit), names=name))
    handle
  }

  # Our argument "expr" is a context constructor
  # that takes some branch targets ("our "ret" and "stp" etc) and
  # returns an entry continuation.
  entry <- expr(return_, ..., bounce=bounce_, bounce_val=bounce_val_,
                stp=stop_, rtn=return_,
                windup=windup_, unwind=unwind_,
                pause=pause_, pause_val=pause_val_,
                trace=trace, setDebug=setDebug, getCont=getCont,
                evl=evl_, sto=sto_,
                registerExit=registerExit, addExit=addExit)
  pumpCont <- entry

  if (using_onexit) {
    doExits %<-% function() {
      if (length(exit_list) > 0) {
        trace_("pump: running on.exit handlers\n")
        first_exit <- exit_list[[1]]
        exit_list[[1]] <<- NULL
        takeExit(first_exit)
      } else {
        trace_("pump: no more on.exit handlers\n")

        switch(after_exit,
               "finish" = {action <<- "finish"; rtn(value)},
               "stop" = {action <<- "stop"; stp(value)},
               "rethrow" = stp("previous error swallowed by on.exit"),
               "xxx" = NULL, # we probably already have an error
               stp(paste0("Unexpected after-exit action: ", action)))
      }
    }

    #And now initialize the exits, pointing them back at doExits
    exit_fns <- vector("list", length(exit_ctors))
    for (i in seq_along(exit_ctors)) {
      name <- names(exit_ctors)[[i]]
      exit_fns[[i]] <- structure(
        exit_ctors[[i]](
          `;_ctor`(paste0(.contextName, ".", name, ";"))(doExits),
          ..., bounce=bounce_, bounce_val=bounce_val_,
          stp=stop_, rtn=return_,
          windup=windup_, unwind=unwind_,
          pause=pause_, pause_val=pause_val_,
          trace=trace, setDebug=setDebug, getCont=getCont,
          evl=evl_, sto=sto_,
          addExit=addExit, registerExit=registerExit))
      assign(name, exit_fns[[i]])
    }

    takeExit %<-% eval(bquote(
      splice=TRUE, function(val)
        switch(val, ..( lapply(names(exit_ctors),
                               \(x) call("bounce_", as.name(x)))))))

  } else {
    doExits %<-% function() {
      switch(after_exit,
             "finish" = {action <<- "finish"; rtn(value)},
             "stop" = {action <<- "stop"; stp(value)},
             "xxx" = NULL, # we probably already have an error raised
             stp(paste0("Unexpected after-exit action: ", action)))
    }
  }

  pump %<g-% function() {
    trace("pump: run\n")
    if(using_onexit) {
      if (action %in% c("exit", "continue", "continue_val")) {
        trace("pump: skipping on.exit\n")
      } else {
        trace("pump: using on.exit\n")
        on.exit({
          trace(paste0("pump: on.exit with action ", action, "\n"))
          if (!action %in%
                c("pause", "pause_val", "finish")) {
            trace(paste0("pump: exiting abnormally: ", action, "\n"))
            action <<- "exit"
            # silly compiler, count these as tailcalls I guess
            pumpCont <<- function() doExits()
            tmp <- (function() pump())()
            switch(action,
                   "finish"=
                     return(tmp), # you OVERRIDE the exit?
                   "pause"=,
                   "pause_val"= {
                     trace("pump: swallowing error?\n")
                     if (after_exit=="xxx") {
                       # so we have not explicitly stopped
                       # as in reaching return_ or stop_
                       after_exit <<- "rethrow"
                       return(tmp) # swallow
                     } else {
                       return(tmp)
                     }
                   },
                   tmp)
          }
        })
      }
    }
    doWindup(runPump)
    while(action == "rewind") {
      doWindup(runPump)
    }
    trace(paste0("pump exiting with action: ", action, "\n"))
    if (identical(value, nonce)) NULL else value
  }

  exit_ctors <- NULL
  exit_fns <- NULL

  runPump %<g-% function() {
    if (debugInternal) debugonce(pumpCont)
    switch(action,
           exit=,
           rewind=,
           pause={action <<- "xxx"; pumpCont()},
           pause_val={action <<- "xxx"; pumpCont(value)},
           stop("pump asked to continue, but last action was ", action))
    repeat switch(action,
             exit=,
             continue={
               trace("pump: continue, really, here\n")
               if (debugInternal) debugonce(pumpCont)
               action <<- "xxx";
               pumpCont()
             },
             continue_val={
               trace("pump: continue with value\n")
               if (debugInternal) debugonce(pumpCont)
               action <<- "xxx";
               pumpCont(value)
             },
             break
             )
    value
  }

  pump
}


on.exit_cps <- function(.contextName,
                        expr,
                        add=R(paste0(.contextName, ".add"), FALSE),
                        after=R(paste0(.contextName, ".after"), TRUE)) {
  function(cont, ..., registerExit, addExit) {
    list(.contextName, cont, registerExit, addExit)

    # at construction time, pass the constructor back up to pump --
    # the current scope handlers don't apply
    handle <- registerExit(expr)
    add_p <- NULL
    after_p <- NULL

    gotAfter %<-% function(val) {
      after_p <<- val
      addExit(cont, handle, add_p, after_p)
    }
    getAfter <- after(gotAfter, ..., registerExit=registerExit, addExit=addExit)
    gotAdd %<-% function(val) {
      add_p <<- val
      getAfter()
    }
    getAdd <- add(gotAdd, ..., registerExit=registerExit, addExit=addExit)
  }
}
