#' @import nseval

# CPS definitions for core R control flow constructs

# debugging constants....
# assign("verbose", TRUE, envir=getNamespace("async"))
verbose <- FALSE
trace <- function(...) if(verbose) cat(..., "\n", sep=" ")

browseOnAssert <- FALSE
# assign("browseOnAssert", TRUE, envir=getNamespace("async"))
assert <- function(condition, msg) {
  if (!isTRUE(condition)) {
    if(browseOnAssert) browser()
    stop(msg)
  }
}

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
# constructor takes a list of callback argument -- one argument "cont"
# representing where to jump after the present computation, and a set
# of named arguments corresponding to other branch points (like break,
# next, return, stop, yield, await, etc.)
#
# When you the topmost context constructor and provide the return
# callback and other top-level callbacks(*), each constructor will
# instantiate its children, propagating its own callbacks. For example
# a "for" loop constructor will construct new "nxt" and "brk"
# callbacks to provide to its children.
#
# (*) The common set of execution callbacks is provided by
# make_pump(); make_gen() and make_async() add their own special
# callbacks to those of make_pump.
#
# The context constructor should returns a single "begin" function. It
# should also instantiate any other functions it requires. Each
# function tail-calls into the next, optionally passing along some
# arguments; each callback thus forms a node on a directed graph of
# execuation. All the necessary nodes should be instantiated by
# the context constructor, so that in a future version, a compiler can
# statically walk over them to extract the call graph.
#
# Because R does not have native tailcall elimination, nodes can
# optionally tailcall into the "ret" callback, which takes a
# continuation function as argument. This sets a thunk which allows
# the stack to unwind and continue from the callback passed to it.
# make_pump() operates a trampoline that makes this work.
#
# "cps_" stood for "continuation passing style," but we are actually now
# building a static graph, rather than passing continuations around as
# arguments at runtime.

`(_cps` <- function(expr) {
  force(expr)
  function(...) {
    # ()'s constructor just forwards to expr's constructor
    expr(...)
  }
}

maybe <- function(x, if_missing=NULL)
  if (!missing_(arg(x))) x else if_missing

make_store <- function(sym) { force(sym)
  function(x, value) { list(x, value)
    function(cont, ..., ret) { list(cont, ret)
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


switch_cps <- function(EXPR, ...) {
  force(EXPR); alts <- list(...)
  function(cont, ..., ret, stop) {
    alts <- lapply(alts, function(x) x(cont, ..., ret=ret, stop=stop))
    got_expr <- function(val) {
      if (is.numeric(val)) {
        branch <- alts[[val]]
        branch()
      } else {
        defaults <- alts[names(alts) == ""]
        if (length(defaults) > 1) {
          stop("Duplicate 'switch' defaults")
        }
        branch <- alts[[val]]
        if (is.null(branch)) {
          if (length(defaults) == 1) {
            defaults[[1]]()
          } else {
            #this actually is what switch does? Wild.
            ret(cont, invisible(NULL))
          }
        } else {
          ret(branch)
        }
      }
    }
    EXPR(got_expr, ..., ret=ret, stop=stop)
  }
}

`;_ctor` <- function(cont, ..., ret) {
  list(cont, ret)
  function(val) {
    force(val)
    val <- NULL #force, then discard
    ret(cont)
  }
}

`{_cps` <- function(...) {
  args <- list(...)
  function(cont, ...) {
    list(cont)
    if (length(args) == 0) {
      function() cont(NULL)
    } else if (length(args) == 1) {
      # just use the inner arg's continuation, like ()
      args[[1]](cont, ...)
    } else {
      # build a chain last to first
      entries <- rep(list(NULL), length(args))
      entries[[length(args)]] <- args[[length(args)]](cont, ...)
      for (i in rev(seq_len(length(args) - 1))) {
        # use force_then to discard results
        entries[[i]] <- args[[i]](`;_ctor`(entries[[i+1]], ...), ...)
      }
      entry <- entries[[1]]
      entries <- NULL
      entry
    }
  }
}

# Here's how sigils and restarts are handled: context constructors
# accept and pass down a list of handlers (...) through the right hand
# side. Of these we have "cont" and "ret" always. If a CPS function
# wants a signal "next" or such, it does that by calling into its
# `nxt` handler.

next_cps <- function()
  function(cont, ..., ret, nxt) {
    if (missing(nxt)) stop("call to next is not in a loop")
    list(ret, nxt)
    function() {
      trace("next")
      ret(nxt)
    }
  }

break_cps <- function()
  function(cont, ..., ret, brk) {
    if (is_missing(brk)) stop("call to break is not in a loop")
    list(ret, brk)
    function() {
      trace("break")
      ret(brk)
    }
  }

# If you want to establish a new target for "break" or "next" you have to 

repeat_cps <- function(expr) { force(expr) #expr getting NULL???
  function(cont, ..., ret, brk, nxt) {
    list(cont, ret)
    again <- function(val) {
      trace("repeat again")
      force(val)
      val <- NULL
      ret(begin)
    }
    brk_ <- function() {
      trace("repeat break")
      ret(cont, invisible(NULL))
    }
    nxt_ <- function() {
      trace("repeat next")
      ret(begin)
    }
    begin <- expr(again, ..., ret=ret, brk=brk_, nxt=nxt_)
    begin
  }
}

while_cps <- function(cond, expr) {
  list(cond, expr)
  function(cont, ..., ret, nxt, brk) {
    list(cont, ret)
    again <- function(val) {
      force(val)
      val <- NULL
      ret(begin)
    }
    brk_ <- function() {
      trace("while break")
      ret(cont, invisible(NULL))
    }
    nxt_ <- function() {
      trace("while next")
      ret(begin)
    }
    doExpr <- expr(again, ..., ret=ret, nxt=nxt_, brk=brk_)
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
      reason <- NULL
      trace("for ", var_, "iterator", deparse(as.list(seq_$state)))
      val <- tryCatch(iterators::nextElem(seq_),
                      error = function(e) {
                        trace("for ", var_, " caught error", conditionMessage(e))
                        stopping <<- TRUE
                        reason <<- e
                      })
      if (stopping) {
        if (identical(conditionMessage(reason), 'StopIteration')) {
          trace("for ", var_, " stopping")
          cont(invisible(NULL))
        } else {
          trace("for ", var_, " throwing: ", conditionMessage(reason))
          stop(reason)
        }
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
    doBody <- expr(`;_ctor`(iterate, ..., ret=ret, nxt=nxt_, brk=brk_, stop=stop),
                   ..., ret=ret, nxt=nxt_, brk=brk_, stop=stop) # our brk_
    getSeq <- seq(gotSeq, ..., ret=ret, nxt=nxt, brk=brk, stop=stop) #not our brk
    begin <- var(gotVar, ..., ret=ret, nxt=nxt, brk=brk, stop=stop) #not our brk
  }
}
