# debugging constants....

browseOnError <- FALSE
# assign("browseOnError", TRUE, envir=getNamespace("async"))
assert <- function(condition,
                   msg=c(
                     "assertion failed: ",
                     deparse(src),
                     if(!is.null(getSrcref(src))) c(
                       "at ", getSrcDirectory(src), "/", getSrcFilename(src),
                       ":", getSrcLocation(src, "line")))) {
  if (!isTRUE(condition)) {
    src <- arg_expr(condition)
    if(browseOnError) recover()
    stop(msg)
  }
}

#' @param prefix Character prefix to print before the trace.
#' @rdname async
#' @export
with_prefix <- function(prefix) function(...) cat(paste0(prefix, ": ", ...), sep="")

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
                      ret=base::stop("unused"),
                      windup=base::stop("unused"),
                      unwind=base::stop("unused"),
                      pause=base::stop("unused"),
                      stop=function(val, ...) base::stop(val, ...),
                      return=function(x)x,
                      trace=trace_,
                      eliminate.tailcalls = TRUE,
                      catch=TRUE) {
  list(expr, stop, return, trace)
  nonce <- (function() NULL)

  action <- "pause" # stopped, pause
  pumpCont <- nonce
  value <- nonce
  err <- nonce

  pause_ <- function(cont) {
    trace("pump: pause (awaiting)\n")
    # this ... business is to carry the value from yield() into the next step,
    # whereas "async" does not provide a value to pause.
    # I may just have it be a state variable.
    pumpCont <<- cont
    action <<- "pause"
  }

  pause_val_ <- function(cont, val) {
    trace("pump: pause (yielding)\n")
    list(cont, val)
    value <<- val
    pumpCont <<- function() cont(value)
    action <<- "pause"
  }

  if (eliminate.tailcalls) {
    bounce_ <- function(cont) {
      if(verbose) trace("pump: bounce\n")
      pumpCont <<- cont
      action <<- "continue"
    }
    bounce_val_ <- function(cont, val) {
      force(val) #force
      if(verbose) trace("pump: bounce with value\n")
      pumpCont <<- function() {
        cont(val)
      }
      action <<- "continue"
    }
  } else {
    bounce_ <- function(cont) {
      if(verbose) trace("pump: fake bounce\n")
      cont()
    }
    bounce_val_ <- function(cont, val) {
      if(verbose) trace("pump: fake bounce with value\n")
      list(cont, val) #force
      cont(val)
    }
  }

  stop_ <- function(val) {
    trace(paste0("pump: stop: ", conditionMessage(val), "\n"))
    err <<- val
    action <<- "stop"
    stop(val)
  }

  return_ <- function(val) {
    trace("pump: return\n")
    force(val)
    value <<- val
    action <<- "finish"
    return(val)
  }

  # We maintain a list of "windings."
  # A "winding" is a function that must tailcall into its args like:
  # null_winding <- function(cont, ...) cont(...)
  # f(cont) that establishes a context,
  # and returning from f(cont), unwinds that context.
  if(catch) {
    base_winding <- function(cont, ...) {
      if(verbose) trace("pump: windup\n")
      tryCatch(cont(...), error=function(err){
        trace("pump: caught error by windup\n")
        stop_(err)
      }, finally=if(verbose) trace("pump: unwind\n"))
    }
  } else {
    base_winding <- function(cont, ...) {
      cont(...)
    }
  }
  windings <- list(base_winding)

  # this needs special handling in walk() because there are TWO
  # function pointers and both need to be treated as nodes.
  windup_ <- function(cont, winding, ...) {
    list(cont, winding, ...)
    if(verbose) trace("pump: Adding to windup list\n")
    outerWinding <- windings[[1]]
    g <- function(...) {
      outerWinding(winding, ...)
    }
    windings <<- c(list(g), windings)
    pumpCont <<- cont
      function() {
      trace("pump: continuing after windup\n")
      cont(...)
    }
    action <<- "rewind"
  }

  unwind_ <- function(cont, ...) {
    if(verbose) trace("pump: removing from windup list\n")
    windings[[1]] <<- NULL
    pumpCont <<- function() {
      if(verbose) trace("pump: continuing after unwind\n")
      cont(...)
    }
    action <<- "rewind"
  }

  doWindup <- function(cont, ...) {
    wind <- windings[[1]]
    wind(cont, ...)
  }

  # Our argument "expr" is a context constructor
  # that takes some branch targets ("our "ret" and "stop" etc) and
  # returns an entry continuation.
  entry <- expr(return_, ..., bounce=bounce_, bounce_val=bounce_val_,
                stop=stop_, return=return_, windup=windup_,
                unwind=unwind_, pause=pause_, pause_val=pause_val_, trace=trace)
  pumpCont <- entry

  pump <- function(...) {
    trace("pump: run\n")
    doWindup(runPump, ...)
    while(action == "rewind") {
      action <<- "pause"
      doWindup(runPump)
    }
    trace(paste0("pump: ", action, "\n"))
    if (!identical(value, nonce)) value
  }

  runPump <- function(...) {
    if (action != "pause")
      base::stop("pump asked to continue, but last action was ", action)
    pumpCont(...) # here's where you inject a return value into a yield?
    while(action == "continue") {
      trace("pump: continue\n")
      action <<- "pause";
      list(pumpCont, pumpCont <<- NULL)[[1]]()
    }
  }

  pump
}
