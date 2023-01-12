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
                      ret=stop("unused"),
                      windup=stop("unused"),
                      unwind=stop("unused"),
                      pause=stop("unused"),
                      goto=stop("unused"),
                      evl=stop("unused"),
                      sto=stop("unused"),
                      stp=structure(\(x)function(x){stop(x); NULL},
                                    globalName="stp", localName="stp"),
                      return=structure(function(x)x, localName="return"),
                      trace=trace_,
                      catch=TRUE,
                      targetEnv) {

  .contextName <- "pump"
  list(expr, stp, return, trace, targetEnv)
  nonce <- (function() NULL)

  action <- "pause" # stopped, pause
  pumpCont <- nonce
  value <- nonce
  debugR <- FALSE
  debugInternal <- FALSE

  setDebug <- structure(function(R=debugR, internal=debugInternal) {
    debugR <<- R
    debugInternal <<- internal
    x <- list(R=R, internal=internal)
    x
  }, localName="setDebug", globalName="setDebug")

  getCont <- structure(function() {
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

  bounce_val_ %<-% function(cont, val) {
    force(val)
    trace("pump: bounce with value\n")
    value <<- val
    pumpCont <<- cont
    action <<- "continue_val"
  }


  stop_ <- structure(function(val) {
    trace(paste0("pump: stop: ", conditionMessage(val), "\n"))
    value <<- val
    action <<- "stop"
    stp(val)
  }, localName="stop_", globalName="stop_")

  return_ <- structure(function(val) {
    trace("pump: return\n")
    force(val)
    value <<- val
    action <<- "finish"
    return(val)
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

  # Our argument "expr" is a context constructor
  # that takes some branch targets ("our "ret" and "stp" etc) and
  # returns an entry continuation.
  entry <- expr(return_, ..., bounce=bounce_, bounce_val=bounce_val_,
                stp=stop_, return=return_,
                windup=windup_, unwind=unwind_,
                pause=pause_, pause_val=pause_val_,
                trace=trace, setDebug=setDebug, getCont=getCont,
                evl=evl_, sto=sto_)
  pumpCont <- entry

  pump <- structure(function() {
    trace("pump: run\n")
    doWindup(runPump)
    while(action == "rewind") {
      action <<- "pause"
      doWindup(runPump)
    }
    trace(paste0("pump: ", action, "\n"))
    if (!identical(value, nonce)) value
  }, localName="pump", globalName="pump")

  runPump <- structure(function() {
    if (debugInternal) debugonce(pumpCont)
    switch(action,
           pause=pumpCont(),
           pause_val=pumpCont(value),
           stop("pump asked to continue, but last action was ", action))
    repeat switch(action,
             continue={
               trace("pump: continue\n")
               if (debugInternal) debugonce(pumpCont)
               action <<- "xxx";
               list(pumpCont, pumpCont <<- NULL)[[1]]()
             },
             continue_val={
               trace("pump: continue with value\n")
               if (debugInternal) debugonce(pumpCont)
               action <<- "xxx";
               list(pumpCont, pumpCont <<- NULL)[[1]](value)
             },
             break
             )
    value
  }, localName="runPump", globalName="runPump")

  pump
}
