# debugging constants....

browseOnError <- FALSE
# assign("browseOnError", TRUE, envir=getNamespace("async"))
assert <- function(condition, msg) {
  if (!isTRUE(condition)) {
    if(browseOnError) recover()
    stop(msg)
  }
}

# assign("verbose", TRUE, envir=getNamespace("async"))
verbose <- FALSE
trace_ <- function(x) if(verbose) cat(x)

#' @param prefix Character prefix to print before the trace.
#' @rdname async
#' @export
with_prefix <- function(prefix) function(...) cat(paste0(prefix, ": ", ...), sep="")

reset <- function(...) list(...)[[1]] #evaluates all args then returns first.

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
                      stop=base::stop,
                      return=base::return,
                      trace=trace_,
                      eliminate.tailcalls = TRUE) {
  list(expr, ..., stop, return)
  nonce <- (function() function() NULL)()

  action <- "pause" # stopped, pause
  cont <- nonce
  value <- nonce
  err <- nonce

  pause_ <- function(cont, ...) {
    if(verbose) trace("pump: set unpause\n")
    list(cont)
    cont <<- function() cont(...)
    action <<- "pause"
  }

  if (eliminate.tailcalls) {
    ret_ <- function(cont, ...) {
      list(cont, ...) #force
      if(verbose) trace("pump: set continue\n")
      cont <<- function() {
        cont(...)
      }
      action <<- "continue"
    }
  } else {
    ret_ <- function(cont, ...) {
      if(verbose) trace("pump: fake ret\n")
      list(cont, ...) #force
      cont(...)
    }
  }

  stop_ <- function(err) {
    trace(paste0("pump: stop: ", conditionMessage(err), "\n"))
    err <<- err
    action <<- "stop"
    stop(err)
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
  base_winding <- function(cont, ...) {
    if(verbose) trace("pump: windup\n")
    tryCatch(cont(...), error=function(err){
      trace("pump: caught error by windup\n")
      if(browseOnError) recover()
      stop_(err)
    }, finally=if(verbose) trace("pump: unwind\n"))
  }
  windings <- list(base_winding)

  windup_ <- function(f, cont, ...) {
    list(f, cont, ...)
    if(verbose) trace("pump: Adding to windup list\n")
    outerWinding <- windings[[1]]
    g <- function(...) {
      outerWinding(f, ...)
    }
    windings <<- c(list(g), windings)
    cont <<- function() {
      if(verbose) trace("pump: continuing after windup\n")
      cont(...)
    }
    action <<- "rewind"
  }

  unwind_ <- function(cont, ...) {
    if(verbose) trace("pump: removing from windup list\n")
    windings[[1]] <<- NULL
    cont <<- function() {
      if(verbose) trace("pump: continuing after unwind\n")
      cont(...)
    }
    action <<- "rewind"
  }

  doWindup <- function(...) {
    windings[[1]](...)
  }

  # "expr" represents the syntax tree, It is a context constructor
  # that takes some context functions ("our "ret" and "stop" etc) and
  # returns an entry continuation.
  entry <- expr(return_, ..., ret=ret_, stop=stop_, return=return_,
                windup=windup_, unwind=unwind_, pause=pause_, trace=trace)
  cont <- entry

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
    assert(action == "pause",
           paste0("pump asked to continue, but last action was ", action))
    cont(...) # here's where you inject a return value into an await
    while(action == "continue") {
      if(verbose) trace("pump: continue\n")
      action <<- "pause"; reset(cont, cont <<- NULL)()
    }
  }

  pump
}
