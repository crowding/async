trace <- function(...) cat(..., "\n")

# We maintain a list of "windings."
# A "winding" is a function that must tailcall into its args like:
# null_winding <- function(cont, ...) cont(...)
# f(cont) that establishes a context,
# and returning from f(cont), unwinds that context.
base_winding <- function(cont, ...) {
  trace("Base windup")
  tryCatch(cont(...), error=function(err){
    trace("Stop fired!")
    stop(err)
  }, finally=cat("Base unwind\n"))
}
windings <- list(base_winding)

do_windup <- function(...) {
  windings[[1]](...)
}

do_windup(cat, "dinglebat!\n")

windup_ <- function(f) {
  force(f)
  trace("Adding to windup list")
  outerWinding <- windings[[1]]
  g <- function(...) {
    outerWinding(f, ...)
  }
  windings <<- c(list(g), windings)
  action <<- "windup"
}

unwind_ <- function() {
  windings[[1]] <<- NULL
}

message_windup <- function(str) {
  force(str)
  function(f, ...) {
    cat("enter", str, "\n")
    on.exit({cat("exit", str, "\n")})
    f(...)
  }
}

windup_(message_windup("one"))
do_windup(cat, "doing it!\n"); cat("\n")

windup_(message_windup("two"))
do_windup(cat, "doing it!\n"); cat("\n")

unwind_()
do_windup(cat, "doing it!\n"); cat("\n")

do_windup(cat, "doing it\n"); cat("\n")

unwind_()
do_windup(cat, "doing it!\n"); cat("\n")


if (FALSE) {
  # fun fact about tryCatch:
  # this infinitely loops:
  repeat tryCatch({cat("one\n"); break},
                  finally={cat("two\n"); next})

  # this breaks once...
  repeat tryCatch({cat("one\n"); next},
                  finally={cat("two\n"); break})

  # this loops infinitely
  (function() {
    repeat tryCatch({cat("one\n"); return("two")},
                    finally={cat("two\n"); next})
  })()

  # this returns invisible NULL:
  (function() {
    repeat tryCatch({cat("one\n"); return("two")},
                    finally={cat("two\n"); break})
  })()

  # but this breaks once:
  repeat tryCatch({cat("one\n"); stop("wat")},
                  finally={cat("two\n"); next})

  # i.e. a break/next/return in the "finally"
  # overrides the exit from the main branch, except if the exit from
  # main branch was a "stop," which dominates.
}
