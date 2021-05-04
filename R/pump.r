# debugging constants....

browseOnAssert <- FALSE
# assign("browseOnAssert", TRUE, envir=getNamespace("async"))
assert <- function(condition, msg) {
  if (!isTRUE(condition)) {
    if(browseOnAssert) browser()
    stop(msg)
  }
}

# assign("verbose", TRUE, envir=getNamespace("async"))
verbose <- FALSE
trace_ <- function(x) if(verbose) cat(x)
#async({...}, trace=with_prefix("myGen"))

#' @param trace Enable verbose logging by passing a function to
#'   `trace`, as in `async(trace=cat, {...})`. `trace` needs to take a
#'   character argument. `trace` should take a character argument.
#'   `async(trace=with_prefix("myAsync"), {...})` will trace the async
#'   to console with the given prefix.  For debugging, you can also do
#'   things like `trace=browser` for "single stepping" through an
#'   async.
#' @rdname async
#' @export
with_prefix <- function(prefix) function(...) cat(prefix, ": ", ...,  sep="")


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

  pause_ <- function(cont) {
    trace("pump: set unpause\n")
    list(cont)
    cont <<- function(...) cont(...)
    action <<- "pause"
  }

  if (eliminate.tailcalls) {
    ret_ <- function(cont, ...) {
      list(cont, ...) #force
      trace("pump: set continue\n")
      cont <<- function() {
        cont(...)
      }
      action <<- "continue"
    }
  } else {
    ret_ <- function(cont, ...) {
      trace("pump: fake ret\n")
      list(cont, ...) #force
      cont(...)
    }
  }

  stop_ <- function(err) {
    trace(paste0("pump: stop: ", as.character(err), "\n"))
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
    trace("pump: windup\n")
    tryCatch(cont(...), error=function(err){
      trace("pump: caught error by windup\n")
      stop_(err)
    }, finally=trace("pump: unwind\n"))
  }
  windings <- list(base_winding)

  windup_ <- function(f, cont, ...) {
    list(f, cont, ...)
    trace("pump: Adding to windup list\n")
    outerWinding <- windings[[1]]
    g <- function(...) {
      outerWinding(f, ...)
    }
    windings <<- c(list(g), windings)
    cont <<- function() {
      trace("pump: continuing after windup\n")
      cont(...)
    }
    action <<- "rewind"
  }

  unwind_ <- function(cont, ...) {
    trace("pump: removing from windup list\n")
    windings[[1]] <<- NULL
    cont <<- function() {
      trace("pump: continuing after unwind\n")
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
    trace("pump: run\n")
    cont(...) # here's where you inject a return value into an await
    while(action == "continue") {
      trace("pump: continue\n")
      action <<- "pause"; reset(cont, cont <<- NULL)()
    }
    trace(paste0("pump: ", action, "\n"))
  }

  pump
}
