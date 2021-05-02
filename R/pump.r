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
                      eliminate.tailcalls = TRUE) {
  list(expr, ..., stop, return)
  nonce <- (function() function() NULL)()

  context_action <<- "none" # or rewind

  action <- "pause" # stopped, pause
  cont <- nonce
  value <- nonce
  err <- nonce

  pause_ <- function(cont) {
    trace(where <- "pause handler")
    list(cont)
    cont <<- function(...) cont(...)
    action <<- "pause"
  }

  if (eliminate.tailcalls) {
    ret_ <- function(cont, ...) {
      trace(where <- "pump ret")
      list(cont, ...) #force
      cont <<- function() {
        trace(where <- "Thunk called")
        cont(...)
      }
      action <<- "continue"
    }
  } else {
    ret_ <- function(cont, ...) {
      trace(where <- "pump fake ret")
      list(cont, ...) #force
      cont(...)
    }
  }

  stop_ <- function(err) {
    trace(where <- "pump stop:", as.character(err))
    err <<- err
    action <<- "stop"
    stop(err)
  }

  return_ <- function(val) {
    trace(where <- "pump got return value")
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
    trace("Base windup")
    tryCatch(cont(...), error=function(err){
      trace("Stop fired!")
      stop_(err)
    }, finally=trace("Base unwind\n"))
  }
  windings <- list(base_winding)

  windup_ <- function(f, cont, ...) {
    list(f, cont, ...)
    trace("Adding to windup list")
    outerWinding <- windings[[1]]
    g <- function(...) {
      outerWinding(f, ...)
    }
    windings <<- c(list(g), windings)
    cont <<- function() {
      trace(where <- "continuing after windup")
      cont(...)
    }
    action <<- "rewind"
  }

  unwind_ <- function(cont, ...) {
    trace("removing from windup list")
    windings[[1]] <<- NULL
    cont <<- function() {
      trace(where <- "continuing after unwind")
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
                windup=windup_, unwind=unwind_, pause=pause_)
  cont <- entry

  pump <- function(...) {
    doWindup(runPump, ...)
    while(action == "rewind") {
      action <<- "pause"
      doWindup(runPump)
    }
    if (!identical(value, nonce)) value
  }

  runPump <- function(...) {
    assert(action == "pause",
           paste0("pump asked to continue, but last action was ", action))
    trace("pump unpause")
    cont(...) # here's where you inject a return value into an await
    while(action == "continue") {
      trace("pump continue")
      action <<- "pause"; reset(cont, cont <<- NULL)()
    }
    trace("pump ", action)
  }

  pump
}
