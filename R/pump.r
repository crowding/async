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

  stop_ <- structure(function(val) {
    trace(paste0("pump: stop: ", conditionMessage(val), "\n"))
    value <<- val
    action <<- "stop"
    doExits()
  }, localName="stop_", globalName="stop_")

  return_ <- structure(function(val) {
    trace("pump: return\n")
    force(val)
    value <<- val
    action <<- "finish"
    doExits()
  }, localName="return_", globalName="return_")

  # We maintain a list of "windings."
  # A "winding" is a function that must tailcall into its "cont" arg. like:
  # null_winding <- function(cont) tryCatch(cont())
  # f(cont) that establishes a context,
  # and returning from f(cont), unwinds that context.
  if(catch) {
    base_winding %<-% function(cont) {
      trace("pump: windup\n")
      tryCatch(cont(), error=function(err){
        trace("pump: caught error by windup\n")
        stop_(err)
      }, finally=trace("pump: unwind\n"))
    }
  } else {
    base_winding <- function(cont) cont()
  }
  windings <- list(base_winding)

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
  after_exit <- NULL

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

  exit_ctors <- list()
  registerExit %<-% function(exit) {
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

  if (length(exit_ctors) > 0) {
    # now that we know how many on.exits there are, create doExits
    doExits %<-% function() {
      if (is.null(after_exit)) after_exit <<- action
      if (length(exit_list) > 0) {
        trace_("pump: running on.exit handlers\n")
        first_exit <- exit_list[[1]]
        exit_list[[1]] <<- NULL
        takeExit(first_exit)
      } else {
        trace_("pump: no more on.exit handlers\n")
        switch(after_exit,
               "finish" = rtn(value),
               "stop" = stp(value),
               "xxx" = NULL, # we probably already have an error raised
               stp(paste0("Unexpected action: ", action)))
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
      switch(action,
             "finish" = rtn(value),
             "stop" = stp(value),
             "xxx" = NULL, # we probably already have an error raised
             stp(paste0("Unexpected action: ", action)))
    }
  }

  pump %<g-% function() {
    trace("pump: run\n")
    if(action != "exit") {
      trace("pump: using on.exit\n")
      on.exit({
        trace(paste0("pump: on.exit with action ", action, "\n"))
        if (!action %in%
              c("pause", "pause_val", "stop", "finish")) {
          trace("pump: exiting abnormally\n")
          bounce_(doExits)
          action <<- "exit"
          pump()
        }
      })
    }
    doWindup(runPump)
    while(action == "rewind") {
      doWindup(runPump)
    }
    trace(paste0("pump: ", action, "\n"))
    if (!identical(value, nonce)) value
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
             continue={
               trace("pump: continue\n")
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
