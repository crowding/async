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

pump <- function(expr, ...) {
  thisPump <- make_pump(expr, ...)
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
                      stp=structure(\(x){stop(x); NULL},
                                    globalName="stp", localName="stp"),
                      rtn=structure(function(x)x,
                                    globalName="rtn", localName="rtn"),
                      trace=trace_,
                      catch=TRUE,
                      targetEnv) {

  .contextName <- "pump"
  list(expr, stp, rtn, trace, targetEnv)
  nonce <- (function() NULL)

  if(getOption("async.verbose")) traceBinding("action", NULL)
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

  globalNode(getCont <- function() {
    # For display, return a string describing the current state.
    g <- attr(pumpCont, "globalName")
    if (is.null(g)) {
      paste0(get0(".contextName", environment(pumpCont), ifnotfound="???"),
             "__", attr(pumpCont, "localName"))
    } else g
  })

  node(pause_ <- function(cont) {
    trace("pump: pause (awaiting)\n")
    pumpCont <<- cont
    action <<- "pause"
  })

  node(pause_val_ <- function(cont, val) {
    trace("pump: pause (yielding)\n")
    pumpCont <<- cont
    value <<- val
    action <<- "pause_val"
  })

  node(bounce_ <- function(cont) {
    trace("pump: bounce\n")
    pumpCont <<- cont
    action <<- "continue"
  })

  node(bounce_val_ <- function(cont, val) {
    force(val)
    trace("pump: bounce with value\n")
    value <<- val
    pumpCont <<- cont
    action <<- "continue_val"
  })

  node(evl_ <- function(cont, val) {
    if(debugR)
      val <- eval(substitute({browser(); x}, list(x=val)), targetEnv)
    else
      val <- eval(val, targetEnv)
    cont(val)
  })

  node(sto_ <- function(cont, where, val) {
    targetEnv[[where]] <- val
    cont()
  })

  node(stop_ <- function(val) {
    trace(paste0("pump: stop: ", conditionMessage(val), "\n"))
    value <<- val
    after_exit <<- "stop"
    bounce_(doExits)
  })

  node(return_ <- function(val) {
    trace("pump: return\n")
    force(val)
    value <<- val
    after_exit <<- "return"
    doExits()
  })

  exit_list <- list()
  if(getOption("async.verbose")) traceBinding("after_exit", NULL)
  after_exit <- "xxx"

  node(addExit <- function(cont, handle, add, after) {
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
  })

  using_onexit <- FALSE
  exit_ctors <- list()

  node(registerExit <- function(exit) {
    using_onexit <<- TRUE
    handle <- length(exit_ctors) + 1
    name <- paste0("exit_", as.character(handle))
    # hold off on calling their constructors
    exit_ctors <<- c(exit_ctors, structure(list(exit), names=name))
    handle
  })

  winding_stack <- list()
  current_winding <- NULL

  # this handler needs special handling in walk() because it takes TWO
  # functions and both need to be treated as nodes.
  node(windup_ <- function(cont, winding) {
    list(cont, winding)
    trace("pump: Adding to windup list\n")
    outerWinding <- current_winding
    g <- function(cont) {
      # Call outer winding first and have it continue to the
      # new winding, which continues to 'cont'
      outerWinding(function() winding(cont))
    }
    winding_stack <<- c(list(current_winding), winding_stack)
    current_winding <<- g
    pumpCont <<- cont
    action <<- "rewind"
  })

  node(unwind_ <- function(cont) {
    trace("pump: removing from windup list\n")
    current_winding <<- winding_stack[[1]]
    winding_stack[[1]] <<- NULL
    pumpCont <<- cont
    action <<- "rewind"
  })

  node(afterExit <- function() {
    trace("pump: afterExit\n")
    switch(after_exit,
           "return" = {action <<- "return"; rtn(value)},
           "stop" = {action <<- "stop"; stp(value)},
           "rethrow" = {action <<- "stop";
             stp("abnormal exit; previous error swallowed during on.exit?")
           },
           "xxx" = NULL, #let an error bubble out?
           {
             stp(paste0("Unexpected after-exit action: ", action))
           })
  })

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

  if (!using_onexit) {
    node(doExits <- function() action <<- "exit")
  } else {

    node(doExits <- function() {
      if (length(exit_list) > 0) {
        trace("pump: running on.exit handlers\n")
        first_exit <- exit_list[[1]]
        exit_list[[1]] <<- NULL
        takeExit(first_exit)
      } else {
        trace("pump: no more on.exit handlers\n")
        action <<- "exit"
      }
    })

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

    node(takeExit <- eval(bquote(
      splice=TRUE, function(val)
        switch(val, ..( lapply(names(exit_ctors),
                               \(x) call("bounce_", as.name(x))))))))

  }


  # We maintain a list of "windings."
  # A "winding" is a function that must tailcall into its "cont" arg. like:
  # null_winding <- function(cont) tryCatch(cont())
  # that establishes a context,
  # and returning from f(cont), unwinds that context.
  if(catch) {
    globalNode(base_winding <- function(cont) {
      trace("pump: base tryCatch\n")
      tryCatch({tmp <- cont(); return(tmp)}, error=function(err){
        trace(paste0("pump: caught error: ",
                     conditionMessage(err), "\n"))
        current_winding <<- function(cont)base_winding(cont)
        action <<- "rewind"
        after_exit <<- "stop"
        value <<- err
        pumpCont <<- function() doExits()
      }, finally=trace("pump: base tryCatch exits\n"))
    })
  } else if (!using_onexit) {
    globalNode(base_winding <- function(cont) cont())
  } else {
    trace("pump: base on.exit\n")
    globalNode(base_winding <- function(cont) {
      on.exit({
        trace(paste0("pump: base on.exit with action ", action, "\n"))
        if (!action %in%
              c("pause", "pause_val", "exit", "stop", "return", "rewind")) {
          trace(paste0("pump: exiting abnormally: ", action, "\n"))
          # silly compiler, count these as tailcalls I guess?
          pumpCont <<- function() doExits()
          current_winding <<- function(cont) base_winding(cont)
          action <<- "rewind"
          after_exit <<- "xxx"
          base_winding(cont) # run the pump here, once
          trace(paste0("pump: after exit handlers, action is ", action, "\n"))
          switch(action,
                 "return" = {
                   action <<- "exit"
                   return(value)
                 },
                 "pause_val"= {
                   after_exit <<- "rethrow"
                   return(value)
                 },
                 "pause"= {
                   # unreachable bc only asyncs use pause(), and those use tryCatch
                   warning("pause in on-exit handler suppresses error")
                   return(NULL)
                 },
                 "exit" = switch(
                   ## this can't be a call to afterExit b/c I need the "return"
                   after_exit,
                   "return" = {action <<- "return"; return(rtn(value))},
                   "stop" = {browser(); action <<- "stop"; stp(value)},
                   "rethrow" = {browser(); action <<- "stop";
                     stp("abnormal exit; previous error suppressed?")
                   },
                   "xxx" = NULL, #let an error bubble out?
                   stp(paste0("Unexpected after-exit action: ", after_exit)) # nocov
                   )
                 )
        }
      })
      cont()
    })
  }

  current_winding <- base_winding

  globalNode(doWindup <- function(cont) {
    tmp <- current_winding
    tmp(cont)
  })

  globalNode(pump <- function() {
    trace("pump: run\n")
    doWindup(runPump)
    while(action == "rewind") {
      doWindup(runPump)
    }
    if (action == "exit") {
      afterExit()
    }
  })

  exit_ctors <- NULL
  exit_fns <- NULL

  globalNode(runPump <- function() {
    if (debugInternal) debugonce(pumpCont)
    switch(action,
           rewind=,
           pause={action <<- "xxx"; pumpCont()},
           pause_val={action <<- "xxx"; pumpCont(value)},
           stop("pump asked to continue, but last action was ", action)) # nocov
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
  })

  pump
}


on.exit_cps <- function(.contextName,
                        expr,
                        add=R(paste0(.contextName, ".add"), FALSE),
                        after=R(paste0(.contextName, ".after"), TRUE)) {
  function(cont, ..., registerExit, addExit) {
    list(.contextName, cont, registerExit, addExit)

    # at construction time, pass the constructor back up to pump --
    # the current scope's handlers don't apply
    handle <- registerExit(expr)
    add_p <- NULL
    after_p <- NULL

    node(gotAfter <- function(val) {
      after_p <<- val
      addExit(cont, handle, add_p, after_p)
    })
    getAfter <- after(gotAfter, ..., registerExit=registerExit, addExit=addExit)
    node(gotAdd <- function(val) {
      add_p <<- val
      getAfter()
    })
    getAdd <- add(gotAdd, ..., registerExit=registerExit, addExit=addExit)
  }
}