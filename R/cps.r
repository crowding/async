#' @import nseval

# CPS definitions for core R control flow constructs

trace <- function(...) NULL
#trace <- function(...) cat(..., "\n")

# The functions with names ending in "_cps" all construct and return
# new functions. The constructors and their arguments correspond to
# the nodes in the syntax tree; that is, a generator like this:
#
## gen( for( i in 1:10 ) yield(i) )
#
# results in a constructor call tree like this:
## make_gen(for_cps(arg_cps(i), arg_cps(1:10), yield_cps(i)))
#
# Each _cps constructor constructs and returns a continuation
# function.  "cps" stands for "continuation passing style" which
# describes how execution of a generator works; one continuation
# passes control to the next.
#
# The _outer_ arguments correspond to things in the R language that
# should result in values.
#
# The cps functions (that is, the inner
# functions constructed and returned) take a list of arguments that
# represent other destinations you may have.  Every function has a
# first argument "cont" representing what to The second argument is
# usually "ret" which is used to eliminate tailcalls.
#
# So, when a continuation function is executing, it has two argument
# lists. The outer, corresponding to the arguments, are all
# continuation functions that should result in a value.
#
# The inner arguments represent execution state, traps, etc. They are
# taken from continuations "above", and are passed to
# continuations "below" (i.e. those representing your arguments)
#
# In true continuation passing style, each continustion function calls
# the next, so that execution state passes from one continuation
# function to the next. Because R does not have tailcall elimination,
# this would endlessly build the stack. So there is a trampoline; one
# of the continuation arguments that is passed around is called
# "ret". Pass "cont" and other continuations to "ret" and return
# normally; this will unwind the stack. This is used to implement
# `while`, `repeat`, `for`, etc. without endlessly growing the
# stack. On the other hand, leaving things on the stack may make
# stacktraces more readable.
#
# CHECK: All in the outer arglist are val-continuation functions.
#    If a function is a val-continuation function, its callbacks
#    must use the first argument name "val".
# CHECK: all in the inner arglist are sequence continuation functions.
#    A sequence continuation function uses the first argument name "cont"
# CHECK: The first argument provided to "ret" should be a
#    sequence-continuation function.???
# CHECK: ... are passed on to all value-continuation functions.
# CHECK: All args are passed by name past the first argument.

# NOTE: lots of stuff uses "ret" unnecessarily?

# `(_cps` is the simplest continuation-passing function: it continues to the "expr" continuation


## `(_cps` <- function(expr) {
##   # in case () needs to force?
##   force(expr)
##   function(cont, ...) {
##     gotVal <- function(val) {
##       force(val)
##       cont(val)
##     }
##     getVal <- expr(gotVal, ...)
##     getVal
##   }
## }


`(_cps` <- function(expr) {
  # () just disappears and returns its argument's continuation.
  force(expr)
  function(...) {
    expr(...)
  }
}

maybe <- function(x, if_missing=NULL)
  if (!missing_(arg(x))) x else if_missing

make_store <- function(sym) function(x, value) { list(x, value)
  function(cont, ..., ret) {
    dest <- NULL
    gotVal <- function(val) {
      val <- arg(val) # lazy
      v <- do_(quo_(sym, env(dest)),
               dest,
               val)
      ret(cont, v)
    }
    getVal <- value(gotVal, ..., ret=ret)
    gotX <- function(val) {
      dest <<- arg(val)
      getVal() <<- arg(val)
    }
    getX <- x(gotX, ..., ret=ret)
    getX
  }
}

`<-_cps` <- make_store(quote(`<-`))
`<<-_cps` <- make_store(quote(`<<-`))

`&&_cps` <- function(left, right) { list(left, right)
  function(cont, ..., ret) {
    leftVal <- NULL
    gotRight <- function(val) {
      trace("&& got right")
      cont(reset(leftVal, leftVal <<- NULL) && val)
    }
    getRight <- right(gotRight, ..., ret=ret)
    gotLeft <- function(val) {
      trace("&& got left")
      if (isFALSE(val)) cont(FALSE) else {
        leftVal <<- val
        getRight()
      }
    }
    getLeft <- left(gotLeft, ..., ret=ret)
    getLeft
  }
}

`||_cps` <- function(left, right) { list(left, right)
  function(cont, ..., ret) {
    leftVal <- NULL
    gotRight <- function(val) {
      trace("|| got right")
      ret(cont, reset(leftVal, leftVal <<- NULL) || val)
      # cont(reset(leftVal, leftVal <<- NULL) || val)
    }
    getRight <- right(gotRight, ..., ret=ret)
    gotLeft <- function(val) {
      trace("|| got left")
      if (isTRUE(val)) {
        ret(cont, TRUE)
        #cont(TRUE)
      } else {
        leftVal <<- val
        getRight()
      }
    }
    getLeft <- left(gotLeft, ..., ret=ret)
    getLeft
  }
}

if_cps <- function(cond, cons.expr, alt.expr) {
  list(cond, cons.expr, maybe(alt.expr))
  function(cont, ..., stop, ret) {
    list(cont, ret)
    if (is_missing(alt.expr)) {
      ifFalse <- function() cont(invisible(NULL))
    } else {
      ifFalse <- alt.expr(cont, ..., stop=stop, ret=ret)
    }
    ifTrue <- cons.expr(cont, ..., stop=stop, ret=ret)
    gotCond <- function(val) {
      if(isTRUE(val)) {
        trace("if true")
        ifTrue()
      }
      else if(isFALSE(val)) {
        trace("if false")
        ifFalse()
      }
      else stop("Invalid condition")
    }
    getCond <- cond(gotCond, ..., stop=stop, ret=ret)
  }
}


switch_cps <- function(EXPR, ...) { force(EXPR); alts <- list(...)
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

force_then <- function(cont, ..., ret) {
  force(cont)
  if (!is.function(cont)) stop()
  function(val) {
    #if(!is_missing(val)) force(val) #prob not necessary?
    force(val)
    val <- NULL #force, then discard
    #cont()
    ret(cont)
  }
}

`{_cps` <- function(...) {
  args <- list(...)
  function(cont, ..., ret) {
    if (length(args) == 0) {
      function() cont(NULL)
    } else if (length(args) == 1) {
      # just use the inner arg's continuation, like ()?
      args[[1]](cont, ..., ret=ret)
    }
    else {
      # build a chain last to first
      entries <- rep(list(NULL), length(args))
      entries[[length(args)]] <- args[[length(args)]](cont, ..., ret=ret)
      for (i in rev(seq_len(length(args) - 1))) {
        # string continuations together with "then"
        entries[[i]] <- args[[i]](force_then(entries[[i+1]], ..., ret=ret), ..., ret=ret)
      }
      entry <- entries[[1]]
      entries <- NULL
      entry
    }
  }
}

# question: is there a situation in which not forcing the arg of () or {}
# is consequential? What about side effects?



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


# Here's how sigils and restarts are handled: continuation functions
# accept and pass down a list of alternate continuations (...) through
# the right hand side. Of these we have "cont" and "ret" always.  When
# a CPS function wants a signal "next" or such, it adds "nxt" to the
# continuation arguments.

next_cps <- function()
  function(cont, ..., ret, nxt) {
    if (missing(nxt)) stop("call to next is not in a loop")
    list(ret, nxt)
    function() {
      trace("Next called")
      ret(nxt)
    }
  }

break_cps <- function()
  function(cont, ..., ret, brk) {
    if (is_missing(brk)) stop("call to break is not in a loop")
    list(ret, brk)
    function() {
      trace("Break called")
      ret(brk)
    }
  }

repeat_cps <- function(expa) { force(expa) #expr getting NULL???
  function(cont, ..., ret, brk, nxt) {
    list(cont, ret)

    brk_ <- function() {
      trace("Breaking from repeat")
      ret(cont, invisible(NULL))
    }

    nxt_ <- function(val) {
      maybe(val) #discard
      val <- NULL
      trace("Next repeat")
      ret(begin)
    }

    begin <- expa(nxt_, ..., ret=ret, brk=brk_, nxt=nxt_)
    begin
  }
}

while_cps <- function(cond, expr) { list(cond, expr)

  function(cont, ..., ret) {
    # we establish "nxt" and "brk" continuations
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
}

#' @import iterators
for_cps <- function(var, seq, expr) { list(var, seq, expr)
  function(cont, ..., ret) {
    trace("for loop started")
    i <- 0
    var_ <- NULL
    env_ <- NULL
    seq_ <- NULL
    value <- NULL
    brk <- function(...) {
      ret(cont, invisible(NULL))
    }
    got_var <- function(val) {
      var_ <<- as.character(arg_expr(val))
      env_ <<- arg_env(val)
      seq(got_seq, ..., ret=ret, brk = brk)
    }
    got_seq <- function(val) {
      trace(paste("for ", var_, "got seq"))
      seq_ <<- iter(val)
      trace("for ", var_, "made iterator", deparse(as.list(seq_$state)))
      iterate()
    }
    iterate <- function() {
      trace("for ", var_, "iterate")
      stopping <- FALSE
      trace("for ", var_, "iterator", deparse(as.list(seq_$state)))
      val <- tryCatch(iterators::nextElem(seq_),
                      error = function(e) {
                        trace("for ", var_, " caught error", conditionMessage(e))
                        stopping <<- TRUE
                        if (identical(conditionMessage(e), 'StopIteration'))
                          NULL else stop(e)
                      })
      if (stopping) {
        trace("for ", var_, "stopping")
        ret(cont, invisible(value))
      } else {
        value <<- NULL
        assign(var_, val, envir=env_)
        trace("for ", var_, "= ", deparse(val))
        expr(got_expr, ..., ret=ret, brk=brk, nxt=nxt)
      }
    }
    got_expr <- function(val) {
      trace("for ", var_, "got value")
      value <<- val
      ret(iterate)
    }
    nxt <- function(...) {
      ret(iterate)
    }
    var(got_var, ..., ret=ret) #just quoting so no context passed
  }
}
