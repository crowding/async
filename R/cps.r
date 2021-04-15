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
# Each _cps constructor returns a "context" constructor.  This second
# constructor takes a list of continuation arguments -- one argument
# "cont" representing where to jump after the present computation, and
# a set of named arguments corresponding to other contextualbranch
# points (like break, next, stop, etc.)
#
# Providing top-level context to the outermost context constructor
# will initialize all the below as well. The result is a graph of
# functions implementing the computation, linked together in
# continuation passing style -- that is, each function calls the next
# in tail position.
#
# In true continuation passing style, each continuation function calls
# the next, so that execution state passes from one continuation
# function to the next. Because R does not have native tailcall
# elimination, this would endlessly build the stack. So there is a
# trampoline; one of the context arguments should be named "ret"; it
# should save a call and arguments and re-start after the stack unwinds.
#
# Providing the execution context and trampoline is done by
# make_pump().

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
      getVal()
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
  function(cont, ..., ret, stop) {
    list(cont, ret, stop)
    if (is_missing(alt.expr)) {
      ifFalse <- function() cont(invisible(NULL))
    } else {
      ifFalse <- alt.expr(cont, ..., ret=ret, stop=stop)
    }
    ifTrue <- cons.expr(cont, ..., ret=ret, stop=stop)
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

repeat_cps <- function(expr) { force(expr) #expr getting NULL???
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
    # nxt_ takes optional val_ because used both ways here:
    begin <- expr(nxt_, ..., ret=ret, brk=brk_, nxt=nxt_)
    begin
  }
}

while_cps <- function(cond, expr) {
  list(cond, expr)
  function(cont, ..., ret, nxt, brk) {
    list(cont, ret)

    brk_ <- function() {
      trace("Breaking from while")
      ret(cont, invisible(NULL))
    }
    nxt_ <- function() {
      trace("Next while")
      ret(begin)
    }
    doExpr <- expr(force_then(nxt_, ..., ret=ret, nxt=nxt, brk=brk),
                   ..., ret=ret, nxt=nxt_, brk=brk_)
    gotCond <- function(val) {
      if (val) {
        val <- NULL
        doExpr()
      } else {
        val <- NULL
        ret(cont, invisible(NULL))
      }
    }
    begin <- cond(gotCond, ..., ret=ret, nxt=nxt_, brk=brk_)
  }
}


#' @import iterators
for_cps <- function(var, seq, expr) {
  list(var, seq, expr)
  function(cont, ..., ret, nxt, brk, stop) {
    list(cont, ret, maybe(nxt), maybe(brk), stop)
    i <- 0
    var_ <- NULL
    env_ <- NULL
    seq_ <- NULL
    brk_ <- function() {
      ret(cont, invisible(NULL))
    }
    nxt_ <- function() {
      ret(iterate)
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
        trace("for ", var_, " stopping")
        ret(cont, invisible(NULL))
      } else {
        assign(var_, val, envir=env_)
        trace("for ", var_, " = ", deparse(val))
        doBody()
      }
    }
    gotSeq <- function(val) {
      seq_ <<- iter(val)
      iterate()
    }
    gotVar <- function(val) {
      var_ <<- as.character(arg_expr(val))
      env_ <<- arg_env(val)
      getSeq()
    }
    doBody <- expr(force_then(iterate, ..., ret=ret, nxt=nxt_, brk=brk_, stop=stop),
                   ..., ret=ret, nxt=nxt_, brk=brk_, stop=stop) # our brk_
    getSeq <- seq(gotSeq, ..., ret=ret, nxt=nxt, brk=brk, stop=stop) #not our brk
    begin <- var(gotVar, ..., ret=ret, nxt=nxt, brk=brk, stop=stop) #not our brk
  }
}
