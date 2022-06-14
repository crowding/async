#' @import nseval

# CPS definitions for core R control flow constructs

# The functions with names ending in "_cps" all construct and return
# new functions. The constructors and their arguments correspond to
# the nodes in the syntax tree; that is, a generator like this:
#
## gen( for( i in 1:10 ) yield(i) )
#
# results in a constructor call tree like this:
## make_gen(for_cps(R(i), R(1:10), yield_cps(i)))
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
    function(cont, ..., ret, trace=trace_) {
      list(cont, ret, trace)
      dest <- NULL
      store <- function(val) {
        if (is.language(val)) {
          val <- as.call(list(quote, val))
        }
        v <- do.call(sym, list(expr(dest), val), envir=env(dest))
        cont(v)
      }
      getVal <- value(store, ..., ret=ret, trace=trace)
      var <- function(val) {
        dest <<- arg(val)
        getVal()
      }
      getVar <- x(var, ..., ret=ret, trace=trace)
      getVar
    }
  }
}

`<-_cps` <- make_store(`<-`)
`<<-_cps` <- make_store(`<<-`)

`&&_cps` <- function(left, right) { list(left, right)
  function(cont, ..., ret, trace=trace_) {
    list(cont, ret, trace)
    leftVal <- NULL
    andRight <- function(val) {
      test <- leftVal && val
      trace(paste0("&&: ", deparse(test), "\n"))
      leftVal <<- NULL
      cont(test)
    }
    ifTrue <- right(andRight, ..., ret=ret, trace=trace)
    andLeft <- function(val) {
      if (isFALSE(val)) {
        trace("&&: FALSE (skip)\n")
        cont(FALSE)
      } else {
        leftVal <<- val
        ifTrue()
      }
    }
    getLeft <- left(andLeft, ..., ret=ret, trace=trace)
    getLeft
  }
}

`||_cps` <- function(left, right) { list(left, right)
  function(cont, ..., ret, trace=trace_) {
    list(cont, ret, trace)
    leftVal <- NULL
    orRight <- function(val) {
      test <- leftVal || val
      trace(paste0("||: ", deparse(test), "\n"))
      cont(test)
    }
    ifFalse <- right(orRight, ..., ret=ret, trace=trace)
    orLeft <- function(val) {
      if (isTRUE(val)) {
        trace("||: TRUE (skip)\n")
        cont(TRUE)
      } else {
        leftVal <<- val
        ifFalse()
      }
    }
    getLeft <- left(orLeft, ..., ret=ret, trace=trace)
    getLeft
  }
}

if_cps <- function(cond, cons.expr, alt.expr) {
  list(cond, cons.expr, maybe(alt.expr))
  function(cont, ..., ret, stop, trace=trace_) {
    list(cont, ret, stop, trace)
    ifTrue <- cons.expr(cont, ..., ret=ret, stop=stop, trace=trace)
    if (missing_(arg(alt.expr))) {
      if_ <- function(val) {
        if (isTRUE(val)) {
          trace("if: TRUE\n")
          ifTrue()
        } else if (isFALSE(val)) {
          cont(invisible(NULL))
        } else stop("if: Invalid condition")
      }
    } else {
      ifFalse <- alt.expr(cont, ..., ret=ret, stop=stop, trace=trace)
      if_ <- function(val) {
        if (isTRUE(val)) {
          trace("if: TRUE\n")
          ifTrue()
        } else if (isFALSE(val)) {
          ifFalse()
        } else stop("if: Invalid condition")
      }
    }
    getCond <- cond(if_, ..., stop=stop, ret=ret, trace=trace)
  }
}


switch_cps <- function(EXPR, ...) {
  force(EXPR); alts <- list(...)
  function(cont, ..., ret, stop, trace=trace_) {
    list(cont, ret, stop, trace)
    alts <- lapply(alts, function(x) x(cont, ..., ret=ret, stop=stop, trace=trace))

    switch_ <- function(val) {
      if (is.numeric(val)) {
        trace(paste0("switch: ", val, "\n"))
        branch <- alts[[val]]
        branch()
      } else if (is.character(val)) {
        defaults <- alts[names(alts) == ""]
        if (length(defaults) > 1) {
          stop("Duplicate 'switch' defaults")
        }
        branch <- alts[[val]]
        if (is.null(branch)) {
          if (length(defaults) == 1) {
            trace(paste0("switch: default (", deparse(val), ")", "\n"))
            defaults[[1]]()
          } else {
            trace(paste0("switch: no branch taken (", deparse(val), ")"))
            #this actually is what switch does? Wild.
            cont(invisible(NULL))
          }
        } else {
          trace(paste0("switch: ", deparse(val), "\n"))
          branch()
        }
      }
    }
    EXPR(switch_, ..., ret=ret, stop=stop, trace=trace)
  }
}

`;_ctor` <- function(cont, ..., ret, trace=trace_) {
  # a semicolon forces its input but discards it
  list(cont, ret, trace)
  `;` <- function(val) {
    force(val)
    val <- NULL #force, then discard
    cont()
  }
}

`{_cps` <- function(...) {
  args <- list(...)
  #args <- args[!(missing_(args))]
  function(cont, ...) {
    force(cont)
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
        # link args together with semicolons
        entries[[i]] <- args[[i]](`;_ctor`(entries[[i+1]], ...), ...)
      }
      entry <- entries[[1]]
      entries <- NULL
      entry
    }
  }
}

# Here's how signals and restarts are handled: context constructors
# accept and pass down a list of handlers (...) through the right hand
# side. Of these we have "cont" and "ret" always. If a CPS function
# wants a signal "next" or such, it does that by calling into its
# `nxt` handler.

next_cps <- function()
  function(cont, ..., ret, nxt, trace=trace_) {
    if (missing_(arg(nxt))) stop("call to next is not in a loop")
    list(ret, nxt, trace)
    if (verbose) {
      next_ <- function() {
        trace("next\n")
        nxt()
      }
    } else nxt
  }

break_cps <- function()
  function(cont, ..., ret, brk, trace=trace_) {
    if (missing_(arg(brk))) stop("call to break is not in a loop")
    list(ret, brk, trace)
    if(verbose) {
      break_ <- function() {
        trace("break\n")
        brk()
      }
    } else brk
  }


# If you want to establish a new target for "break" or "next" you
# pass that down to constructor arguments:

repeat_cps <- function(expr) { force(expr) #expr getting NULL???
  function(cont, ..., ret, brk, nxt, trace=trace_) {
    list(cont, ret, maybe(brk), maybe(nxt), trace)
    repeat_ <- function(val) {
      trace("repeat: again\n")
      force(val)
      val <- NULL
      ret(begin)
    }
    brk_ <- function() {
      trace("repeat: break\n")
      cont(invisible(NULL))
    }
    nxt_ <- function() {
      trace("repeat: next\n")
      ret(begin)
    }
    begin <- expr(repeat_, ..., ret=ret, brk=brk_, nxt=nxt_, trace=trace)
    begin
  }
}

while_cps <- function(cond, expr) {
  list(cond, expr)
  function(cont, ..., ret, nxt, brk, trace=trace_) {
    list(cont, ret, maybe(nxt), maybe(brk), trace)
    again <- function(val) {
      force(val)
      val <- NULL
      ret(begin)
    }
    brk_ <- function() {
      trace("while: break\n")
      ret(cont, invisible(NULL))
    }
    nxt_ <- function() {
      trace("while: next\n")
      ret(begin)
    }
    doExpr <- expr(again, ..., ret=ret, nxt=nxt_, brk=brk_, trace=trace)
    while_ <- function(val) {
      if (val) {
        trace("while: do\n")
        val <- NULL
        doExpr()
      } else {
        trace("while: end\n")
        val <- NULL
        ret(cont, invisible(NULL))
      }
    }
    begin <- cond(while_, ..., ret=ret, nxt=nxt_, brk=brk_, trace=trace)
  }
}


#' @import iterators
for_cps <- function(var, seq, expr) {
  list(var, seq, expr)
  function(cont, ..., ret, nxt, brk, stop, trace=trace_) {
    list(cont, ret, maybe(nxt), maybe(brk), stop, trace)
    i <- 0
    var_ <- NULL
    env_ <- NULL
    seq_ <- NULL
    brk_ <- function() {
      cont(invisible(NULL))
    }
    nxt_ <- function() {
      ret(do)
    }
    do <- function() {
      stopping <- FALSE
      reason <- NULL
      trace(paste0("for ", var_, ": next\n"))
      val <- async::nextElemOr(seq_, stopping <- TRUE)
      if (stopping) {
        trace(paste0("for ", var_, ": finished\n"))
        cont(invisible(NULL))
      } else {
        trace(paste0("for ", var_, ": do\n"))
        assign(var_, val, envir=env_)
        body()
      }
    }
    body <- expr(`;_ctor`(do, ..., ret=ret, nxt=nxt_, brk=brk_, stop=stop, trace=trace),
                 ..., ret=ret, nxt=nxt_, brk=brk_, stop=stop, trace=trace) # our brk_
    in_ <- function(val) {
      seq_ <<- iteror(val)
      do()
    }
    getSeq <- seq(in_, ..., ret=ret, nxt=nxt, brk=brk, stop=stop, trace=trace) #not our brk
    for_ <- function(val) {
      var_ <<- as.character(arg_expr(val))
      env_ <<- arg_env(val)
      getSeq()
    }
    begin <- var(for_, ..., ret=ret, nxt=nxt, brk=brk, stop=stop, trace=trace) #not our brk
  }
}


#' Asynchronous pause.
#'
#' "delay" returns a promise which resolves only after the specified
#' number of seconds. This uses the R event loop via [later].
#' In an `[async]` construct you can use `await(delay(secs))` to yield
#' control, for example if you need to poll in a loop
#'
#' @import later later
#' @param secs The promise will resolve after at least this many seconds.
#' @param expr The value to resolve with; will be forced after the delay.
#' @return An object with class ["promise"][promises::promise].
#' @export
#' @examples
#' # print a message after a few seconds
#' async({await(delay(10)); cat("Time's up!\n")})
delay <- function(secs, expr=NULL) {
  promise(function(resolve, reject) {
    later(function() resolve(expr), delay=secs)
  })
}
