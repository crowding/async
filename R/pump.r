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
                      stop=base::stop,
                      return=base::return,
                      eliminate.tailcalls = TRUE) { list(expr, ...)
  nonce <- (function() function() NULL)()

  action <<- "pause" # windup, unwind, stopped, pause
  cont <- nonce
  value <- nonce
  err <- nonce
  windings <- list()

  pause <- function(cont) {
    trace(where <- "pause handler")
    list(cont)
    cont <<- function(...) cont(...)
    action <<- "pause"
  }

  if (eliminate.tailcalls) {
    ret <- function(cont, ...) {
      trace(where <- "pump ret")
      list(cont, ...) #force
      cont <<- function() {
        trace(where <- "Thunk called")
        cont(...)
      }
      action <<- "continue"
    }
  } else {
    ret <- function(cont, ...) {
      trace(where <- "pump fake ret")
      list(cont, ...) #force
      cont(...)
    }
  }

  windup <- function(..., finally) {
    # error handlers are given to us as functions, finally as a continuation
    # need to return a continuation....
    action <<- "windup"
  }

  unwind <- function(..., finally) {
    action <<- "unwind"
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

  # "expr" represents the syntax tree, It is a context constructor
  # that takes some context functions ("our "ret" and "stop" etc) and
  # returns an entry continuation.
  entry <- expr(return_, ..., ret=ret, stop=stop_, return=return_,
                windup=windup, unwind=unwind, pause=pause)
  cont <- entry

  pump <- function(...) {
    if (action == "pause") {
      #action <<- "continue"
      cont(...) # here's where you inject a return value into an await
    } else {
      stop("asked to continue but pump was not paused")
    }
    trace(where <- "pump first return")
    repeat
      switch(action,
             continue={
               trace("pump is continuing")
               action <<- "pause"; reset(cont, cont <<- NULL)()
             },
             pause=,
             finish=,
             stop={
               trace("pump ", action)
               break
             },
             windup=,
             unwind={
               stop("pump wind/unwind not implemented")
             })
    if (!identical(value, nonce)) value
  }

  #debug(pump)
  pump
}




