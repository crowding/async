#' @import nseval

log <- function(...) NULL
log <- print

# The first group of arguments corresponds to expressions in the
# language. They are continuation functions that provide their
# callback with a value.

# The second group of arguments correspond to control flow constructs,
# or places the code can branch to.  For example, if a continuation
# function is in the middle of a loop, the second arglist should be
# provided with "nxt" and "brk" arguments.

# CHECK: All in the left arglist are val-continuation functions.
#    If a function is a val-continuation function, its callbacks
#    must use the first argument name "val".
# CHECK: all in the right arglist are sequence continuation functions.
#    A sequence continuation function uses the first argument name "cont"
# CHECK: The first argument provided to "ret" should be a
#    sequence-continuation function.?
# CHECK: ... are passed on to all value-continuation functions.
# CHECK: All args are passed by name past the first argument.

# `(_cps` is the simplest continuation-passing function: it continues to its value 

generator_builtins <- c(
  ")", "<-", "<<-", "&&", "||", "if", "switch", "{",
  "next", "break", "yield", "block", "repeat", "while", "for")

`(_cps` <- function(expr) {
  log(where <- "(_cps outer")
  function(cont, ..., ret) {
    log(where <- "(_cps inner")
    expr(function(val) {
      log(where <- "(_cps callback")
      force(val)
      log(where <- "(_cps forced")
      ret(cont, val)
    }, ..., ret=ret)
}}

# arg_cps interfaces between normal R evluation and the CPS system.
# I clone its unevaluated arg and feed that to the continuation.
arg_cps <- function(x) {
  log(where <- "arg_cps outer")
  x <- arg(x) #capture the lazy arg
  function(cont,
           ret = function(cont, ...) cont(...),
           stop = base::stop, ...) {
    log(where <- "arg_cps inner")
    do(cont, x)

    ## Hold up does it make sense at all to use tryCatch in continuation functions?
    ## No, only from pump? Then how would tryCatch catch natural errors?
    ## Maybe we should explicitly force?
      ## out <- tryCatch({
      ##   where <- log("arg_cps: trycatch starting")
      ##   do(cont, x)
      ## }, #okay this is kind of wild, in that x might be
      ##   #forced after "cont" thunks a return, in which
      ##   #case this will not catch. So the pump has to
      ##   #establish an error handler too? Or, well, R has the same issue
      ##   #with exceptions vs lazy eval.
      ## error = function(...) {
      ##   log(where <- "arg_cps: trycatch error")
      ##   log(deparse(list(...)), "\n")
      ##   ret(stop, ...)
      ## })
      ## log(where <- "arg_cps: trycatch finished")
      ## out
      }
}

## arg_cps <- function(...) {
##   where <- log("arg_cps outer")
##   cont <- NULL
##   continue <- function() {
##     ehere <- log("arg_cps inner 2")
##     cont(...)
##   }
##   function(cont, ...) {
##     cont <<- cont
##     where <- log("arg_cps inner")
##     continue() # get away from execution context...
##   }
## }


# Here's an idea for sigils and restarts: you traditionally pass "..."
# down through the stack alongside "cont". When construct want a
# signal "next" or such, it adds "next" to the continuation
# arguments. Traditionally "ret" is one of these.

# Note on handling of ...; the rule is that you should
# always pass ... "down" the stack into val-continuations
# but reject "..." from val-continuations
# "below" you on the syntax tree. Discard ... when returning
# via ret().

store <- function(sym) function(x, value) function(cont, ret, ...) {
  dest <- NULL
  got_x <- function(val) {
    dest <<- arg(val)
    value(got_value, ret=ret, ...)
  }
  got_value <- function(val) {
    val <- arg(val) # lazy
    v <- do_(quo_(sym, env(dest)),
             dest,
             val)
    ret(cont, v)
  }
  x(got_x, ret=ret, ...)
}

`<-_cps` <- store(quote(`<-`))
`<<-_cps` <- store(quote(`<<-`))

`&&_cps` <- function(left, right) function(cont, ret, ...) {
  #get the lval, then
  got_left <- function(val) {
    if(val) {
      # continue
      right(got_right, ret=ret, ...)
    } else {
      ret(cont, FALSE)
    }
    # *when we return using "val" it should be to a function that had
    # *us in the left arglist.
  }
  got_right <- function(val) {
    if(val) {
      ret(cont, TRUE)
    } else {
      ret(cont, FALSE)
    }
  }
  left(got_left, ret=ret, ...)
}

`||_cps` <- function(left, right) function(cont, ret, ...) {
  #get the lval, then
  got_left <- function(val) {
    if(val) {
      ret(cont, TRUE)
    } else {
      right(got_right, ret=ret, ...)
    }
  }
  got_right <- function(val) {
    if(val) {
      ret(cont, TRUE)
    } else {
      ret(cont, FALSE)
    }
  }
  left(got_left, ret=ret, ...)
}

if_cps <- function(cond, cons.expr, alt.expr) function(cont, ret, ...) {
  force(ret)
  got_cond <- function(val) {
    if (val) {
      cons.expr(got_branch, ret=ret, ...)
    } else {
      if (nseval:::is_missing(alt.expr)) {
        ret(cont, invisible(NULL))
      } else {
        alt.expr(got_branch, ret=ret, ...)
      }
    }
  }
  got_branch <- function(val) {
    if (missing(val)) {
      ret(cont, invisible(NULL))
    } else {
      ret(cont, val)
    }
  }
  cond(got_cond, ret=ret, ...)
}

switch_cps <- function(EXPR, ...) {
  alts <- list(...) #collect continuation functions
  function(cont, ..., ret, stop) {
    got_expr <- function(val) {
      if (is.numeric(val)) {
        branch <- alts[[val]]
        alts(cont, ..., ret=ret, stop=stop)
      } else {
        alt <- alts[[val]]
        if (is.null(alt)) {
          #default
          default <- alts[[names(alts) == ""]]
        } else {
          ret(alt, cont)
        }
        if (i %in% names(alts)) {
          default <- alts[[names(alts) == ""]]
        } else {
          defaults <- alts[names(alts) == ""]
          if (length(defaults) == 1) {
            defaults[[1]](cont, ..., ret=ret, stop=stop)
          } else if (length(defaults) == 0) {
            #this actually is what switch does? Wild.
            ret(cont, invisible(NULL))
          } else {
            ret(stop, "Duplicate 'switch' defaults")
          }
        }
      }
    }
    done <- function(val) {
      ret(val)
    }
    EXPR(got_expr, ret, ...)
  }
}

#*  All the entries in both lists are continuation functions.
# call something you got from the LEFT arglist, you give it a
# continuation-function that expects "val".

`{_cps` <- function(...) {
  args <- list(...) # catch continuation-level functions for all args;
                    # this does not force the code-level arguments.
  function(cont, ret, ...) {
  i <- 0;
  n <- length(args);
  iterate <- function(val) {
    i <<- i + 1
    if (i <= n) {
      args[[i]](got_val, ret=ret, ...)
    } else {
      ret(cont, invisible(val))
    }
  }
  got_val <- function(val) {
    if (missing(val)) {
      val <- NULL
    }
    ret(iterate, val)
  }
  iterate(NULL)
}}

# In CPS style, some functions take another CPS function as first
# positional argument. These are called (or returned) to sequence the flow of
# control.  flow. Conventionally I will use the first argument name
# "cont" for these functions.

# Some functions in CPS style take a domain value (a returned value)
# as first positional argument; these functions are called to "return"
# values. I will use the argument name "val" for these
# functions. Generally a "val" function does not receive "..."
# arguments other than "ret".

# In either case, whether calling or thunking, always pass the first
# argument positionally and not named.

# The rest of the arguments should be passed with names. They must
# include "ret" which is used to set up a thunk and unwind the
# stack. By using "ret(cont, <value>)" instead of cont(value), this
# will set up the call that will be made when your function exits,
# then return the value ret() gives you.  Note that "ret" will force
# all its arguments, while making a non-thunked call allows lazy
# arguments to pass through; for this reason, `cps_expr` calls cont
# directly rather than use ret.

# CHECK: Ensure that all arguments other than the first are passed by name.
# CHECK: Ensure that ... are discarded when receiving a "val"

next_cps <- function() function(cont, ..., ret, nxt) {
  if (missing(nxt)) stop("next called, but we do not seem to be in a loop")
  ret(nxt)
}

break_cps <- function() function(cont, ..., ret, brk) {
  if (missing(brk)) stop("break called, but we do not seem to be in a loop")
  ret(brk)
}

yield_cps <- function(expr) function(cont, ..., ret, yield) {
  log("Yield called")
  if (missing(yield)) stop("yield called, but we do not seem to be in a generator")
  got_val <- function(val) {
    log("Got a yield value")
    ret(yield, cont, val)
  }
  expr(got_val, ..., ret=ret, yield=yield)
}

block_cps <- function(expr) function(cont, ..., ret, block) {
  if (missing(block)) stop("block called, but we do not seem to be in a delay")

  got_val <- function(...) ret(block, cont, ...)

  if (nseval:::is_missing(expr)) {
    ret(block, cont)
  } else {
    expr(got_val, ..., ret=ret, block=block)
  }
}

repeat_cps <- function(expr) function(cont, ..., ret) {
  brk <- function(...) {
    # because the signal comes "from below" we discard ... and continue
    ret(cont, invisible(NULL))
  }
  nxt <- function(val) {
    expr(then, ret=ret, nxt=nxt, brk=brk, ...)
  }
  then <- function(val) {
    ret(nxt, NULL)
  }
  nxt(NULL)
}

resolve_cps <- function(expr) function(cont, ..., ret, resolve) {
  if (missing(resolve)) stop("resolve called but we do not seem to be in a delay")
  got_val <- function(...) ret(resolve, cont, ...)
  expr(got_val, ..., ret=ret, resolve=resolve)
}

while_cps <- function(cond, expr) function(cont, ..., ret) {
  # we establish "nxt" and "brk" continuations
  # eval the condition, establishing cont and break continuations.
  value <- NULL
  get_cond <- function() {
    cond(check_run, ret=ret, nxt=nxt, brk=brk, ...)
  }
  check_run <- function(val) {
    if (val) {
      expr(got_expr, ret=ret, nxt=nxt, brk=brk, ...)
    } else {
      ret(cont, value)
    }
  }
  got_expr <- function(val) {
    value <<- val
    ret(get_cond)
  }
  brk <- function(...) {
    ret(cont, NULL)
  }
  nxt <- function(...) {
    ret(get_cond)
  }
  get_cond()
}

for_cps <- function(var, seq, expr) function(cont, ..., ret) {
  i <- 0
  n <- 0
  value <- NULL
  brk <- function(...) {
    ret(cont, invisible(NULL))
  }
  got_var <- function(val) {
    var <<- arg(val) # lazy, also determines scope of assignment
    seq(got_seq, ..., ret=ret, brk = brk)
  }
  got_seq <- function(val) {
    log(paste("for got seq", deparse(val), "length", length(val)))
    seq <<- val
    n <<- length(seq)
    iterate()
  }
  iterate <- function() {
    log("for iterate")
    i <<- i + 1
    if (i <= n) {
      value <<- NULL
      assign(as.character(nseval::expr(var)), seq[[i]], envir=env(var))
      expr(got_expr, ..., ret=ret, brk=brk, nxt=nxt)
    } else {
      ret(cont, invisible(value))
    }
  }
  got_expr <- function(val) {
    log("for got value")
    value <<- val
    ret(iterate)
  }
  nxt <- function(...) {
    ret(iterate)
  }
  var(got_var, ..., ret=ret) #just quoting so no context passed
}
