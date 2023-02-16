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
# execuation. All the necessary nodes should be instantiated by the
# context constructor, so that the compiler can walk over them without
# executing to extract the call graph.
#
# Because R does not have native tailcall elimination, nodes can
# optionally tailcall into the "bounce" callback, which takes a
# destination node as argument. This allows the stack to unwind and
# continue to the node passed to it.  make_pump() operates this
# trampoline.
#
# "cps_" stands for "continuation passing style," which describes the
# way that pausing works; callbacks like "bounce", "await", "windup"
# and "yield" take a continuation argument which represents the future
# of the computation representing to the next step in the
# computation. However, continuation arguments are only used in
# handlers, and not for control flow or indirection; most movement
# between nodes is by ordinary tail calls, the better to form a call
# graph the compiler can collect.
#
# This code is directly executed for interpreted asyncs (with
# `asyncOpts(compileLevel=0)`, but these functions are also the input
# for compiled asyncs. Compiling R generally is impossible, so
# therefore there are some rules on how the nodes below are written,
# call these restrictions `R--`:
#
# * Generally avoid non-standard evaluation. Anything that "looks like" variable
#   references or function calls should actually correspond to
#   variables/functions the compiler can find.
# * Avoid passing a function as an argument. In `tryCatch(stuff,
#   err=myRecovery)` it looks to the compiler like myRecovery is a
#   var.  Do `tryCatch(stuff, err=function(e) myRecovery(e))` instead.
# * Nodes may close over variables in the immediately enclosing environment
#   (the nodes' "context"), any references to outside package code must be
#   fully qualified.
# * Trampoline handlers are recognized by having an argument called "cont."
#   They should be called in "tail position".
# * Don't make a local variable the same name as a variable you close over.
# * Any calls in "tail position" or using trampoline handlers in tail position
#   are treated as part of the graph. Don't put a call in tail position unless
#   you intend that function to be "included" in the graph.
# * You can have helper functions (i.e. that you don't tailcall) defined in
#   the same context as your nodes, and they will be carried to the compiled
#   scope. However, the graph walker does not inspect them to see what
#   variables a helper function uses, unless you explicitly put that function
#   in the start set.
# * No early returns -- a return() placed in the middle of a
#   function doesn't do the same thing when functions are spliced together.

`%<-%` <- function(to, from) {
  envir <- arg_env(to)
  to <- as.character(arg_expr(to))
  if (!identical(envir, environment(from)))
    stop(paste0("bad use of %<-% at ",
                get0(".contextName", envir, ifnotfound="???"),
                "__", to))
  else assign(to, structure(from, localName=to), envir=envir)
}

named <- function(assignment) {
  ar <- arg(assignment)
  assert(identical(expr(ar)[[1]], quote(`<-`)))
  expr(ar)[[1]] <- quote(`%<-%`)
  value(ar)
}

`%<g-%` <- function(to, from) {
  envir <- arg_env(to)
  to <- as.character(arg_expr(to))
  if (!identical(envir, environment(from)))
    stop(paste0("bad use of %<g-% at ", to))
  else assign(to, structure(from, localName=to, globalName=to), envir=envir)
}

globalNamed <- function(assignment) {
  ar <- arg(assignment)
  assert(identical(expr(ar)[[1]], quote(`<-`)))
  expr(ar)[[1]] <- quote(`%<g-%`)
  value(ar)
}

# R() wraps a user-level R expression into an execution node
R <- function(.contextName, x) {
  x <- arg_expr(x)

  function(cont, ..., evl, trace=trace_) {
    list(cont, evl)
    if (is_missing(x)) {
      named(eval_ <- eval(bquote(function() cont(.(quote(expr=))))))
    } else if (!is.language(x)) {
      named(eval_ <- eval(bquote(function() cont(.(x)))))
    } else {
      expr_ <- x
      named(eval_ <- function() {
        trace(paste0("R: ", deparse(expr_), "\n"))
        evl(cont, expr_)
      })
    }
  }
}

#these accessors are used during construction of `for` and `<-` nodes
is_R <- function(f) {
  exists("eval_", environment(f), inherits=FALSE) &&
    identical(f, get("eval_", environment(f)))
}

R_expr <- function(f) {
  if (is_missing_("x", environment(f))) missing_value()
      else get("x", environment(f))
}

`(_cps` <- function(.contextName, expr) {
  list(.contextName, expr)
  function(...) {
    # ()'s constructor just forwards to expr's constructor
    expr(...)
  }
}

maybe <- function(x, if_missing=NULL)
  if (!missing_(arg(x))) x else if_missing

make_store <- function(sym) { force(sym)
  function(.contextName, x, value) { list(.contextName, x, value)
    function(cont, ..., evl, trace=trace_) { list(cont, trace)

      #quote the LHS at constuction time
      lhs <- x(cont, ..., evl=evl, trace=trace)
      if (!is_R(lhs)) {
        stop("Unexpected pause on LHS of <- or <<-")
      }
      var_ <- R_expr(lhs)
      sym <- sym

      named(store <- function(val) {
        if (is.language(val))
          val <- call("quote", val)
        val <- as.call(pairlist(sym, var_, val))
        evl(cont,val)
      })
      value(store, ..., evl=evl, trace=trace)
    }
  }
}

`<-_cps` <- make_store(`<-`)
`<<-_cps` <- make_store(`<<-`)

`&&_cps` <- function(.contextName, left, right) {
  list(.contextName, left, right)
  function(cont, ..., trace=trace_) {
    list(cont, trace)
    leftVal <- NULL
    named(andRight <- function(val) {
      val <- leftVal && val
      trace(paste0("&&: ", deparse(val), "\n"))
      leftVal <<- NULL
      cont(val)
    })
    ifTrue <- right(andRight, ..., trace=trace)
    named(andLeft <- function(val) {
      if (isFALSE(val)) {
        trace("&&: FALSE (skip)\n")
        cont(FALSE)
      } else {
        leftVal <<- val
        ifTrue()
      }
    })
    getLeft <- left(andLeft, ..., trace=trace)
    getLeft
  }
}

`||_cps` <- function(.contextName, left, right) { list(.contextName, left, right)
  function(cont, ..., trace=trace_) {
    list(cont, trace)
    leftVal <- NULL
    named(orRight <- function(val) {
      val <- leftVal || val
      trace(paste0("||: ", deparse(val), "\n"))
      cont(val)
    })
    ifFalse <- right(orRight, ..., trace=trace)
    named(orLeft <- function(val) {
      if (isTRUE(val)) {
        trace("||: TRUE (skip)\n")
        cont(TRUE)
      } else {
        leftVal <<- val
        ifFalse()
      }
    })
    getLeft <- left(orLeft, ..., trace=trace)
    getLeft
  }
}

if_cps <- function(.contextName, cond, cons.expr, alt.expr) {
  list(.contextName, cond, cons.expr, maybe(alt.expr))
  function(cont, ..., stp, trace=trace_) {
    list(cont, stp, trace)
    ifTrue <- cons.expr(cont, ..., stp=stp, trace=trace)
    if (missing_(arg(alt.expr))) {
      named(if_ <- function(val) {
        if(val) ifTrue() else cont(invisible(NULL))
      })
    } else {
      ifFalse <- alt.expr(cont, ..., stp=stp, trace=trace)
      named(if_ <- function(val) {
        if(val) ifTrue() else ifFalse()
      })
    }
    getCond <- cond(if_, ..., stp=stp, trace=trace)
  }
}


switch_cps <- function(.contextName, EXPR, ...) {
  list(.contextName, EXPR)
  alts <- list(...)
  function(cont, ..., stp, goto, bounce, bounce_val, trace=trace_) {
    list(cont, stp, trace, bounce, bounce_val)
    maybe(goto)

    named(goto_ <- function(val) {
      if (is.null(val)) bounce(EXPR) else bounce_val(switch_, val)
    })

    # for the compiler to see the branches they need to be bound to names
    for (i in seq_along(alts)) {
      alts[[i]] <- alts[[i]](cont, ..., stp=stp,
        bounce=bounce, bounce_val=bounce_val, trace=trace, goto=goto_)
      assign(paste0("alt", i), alts[[i]], envir=environment())
    }

    if (is.null(names(alts))) {
      # set up a numeric switch
      branches <- seq_along(alts)

      named(switch_ <- eval(bquote(splice=TRUE, function(val) {
        trace("switch: numeric\n")
        if (!is.numeric(val))
          stp(simpleError(paste0("switch: expected numeric, got ", mode(val))))
        else if (val < 1)
          stp("switch: argument out of bounds")
        else {
          branch <- branches[[val]]
          if (is.null(branch))
            stp(simpleError(paste0("Switch: expected numeric, got ", mode(val))))
          else switch(branch, ..(lapply(seq_along(branches),
                                        function(i)call(paste0("alt", i)))))
        }
      })))
    } else {
      # Construct a character switch
      branches <- new.env()
      default <- NULL
      last <- NULL
      for (i in rev(seq_along(alts))) { # go back to front
        branchName <- names(alts)[[i]]
        if (names(alts)[[i]] == "") {
          if (is.null(default)) {
            default <- i
          }
          else stop("Duplicate 'switch' defaults")
        } else {
          branches[[names(alts)[[i]]]] <- i
          if (is_R(alts[[i]])
              && identical(R_expr(alts[[i]]), quote(expr=))
              && !is.null(last)) {
            # empty switch arg, fallthrough
            branches[[names(alts)[[i]]]] <- last
          } else {
            branches[[names(alts)[[i]]]] <- i
            last <- i
          }
        }
      }

      switchcall <- bquote(
        splice=TRUE,
        switch(branch, ..(lapply(seq_along(alts),
                                 function(i)call(paste0("alt", i))))))
      named(switch_ <- eval(bquote(function(val) {
        trace("switch: character\n")
        if (!is.character(val))
          stp(simpleError(paste0("switch: expected character, got ", mode(val))))
        else .(if (is.null(default)) bquote({
          branch <- get0(val, branches, ifnotfound=NULL)
          if (is.null(branch))
            stp(simpleError(paste0("Switch: branch not found, with no default: `",
                        as.character(val), "`")))
          else .(switchcall)
        }) else bquote({
          branch <- get0(val, branches, ifnotfound=.(default))
          .(switchcall)
        })
        )
      })))
    }
    EXPR <- EXPR(switch_, ..., stp=stp, trace=trace, goto=goto,
                 bounce=bounce, bounce_val=bounce_val)
  }
}

#' Coroutine switch with delimited goto.
#'
#' The `switch` function implemented for coroutines in the `async`
#' package is more strict than the one in base R. In a coroutine,
#' `switch` will always either take one of the given branches or throw
#' an error, whereas base R `switch` will silently return NULL if no
#' branch matches switch argument. Otherwise, the same conventions
#' apply as [base::switch()] (e.g. empty switch branches fall through;
#' a character switch may have one unnamed argument as a default.)
#'
#' Coroutine `switch` also supports a delimited form of `goto`. Within
#' a branch, `goto("other_branch")` will stop executing the present
#' branch and jump to the named branch.  Calling `goto()` without
#' arguments will jump back to re-evaluate the switch expression.
#'
#' If a `goto` appears in a try-finally call, as in:
#'
#'     switch("branch",
#'        branch=tryCatch({...; goto("otherBranch")},
#'                        finally={cleanup()}),
#'        otherBranch={...}
#'     )
#'
#' the `finally` clause will be executed _before_ switching to the new branch.
#' @export
#' @rdname switch
#' @param branch A character string naming the new branch. If missing or NULL,
#'   jumps back to re-evaluate the switch argument.
goto <- function(branch=NULL) {
  stop("`goto` used outside of a 'gen` or `async`.")
}

goto_cps <- function(.contextName, branch=R(NULL)) {
  force(.contextName)
  function(cont, ..., goto) {
    if(missing(goto)) stop("`goto` used outside of a `switch` statement.")
    list(cont, goto)

    named(goto_ <- function(val) goto(val))
    branch <- branch(goto_, ..., goto=goto)

    if (is_R(branch) && identical(R_expr(branch), quote(expr=))) {
      named(goto_ <- function(val) goto(NULL))
    } else {
      branch
    }
  }
}

`;_ctor` <- function(.contextName) {
  force(.contextName)
  # a `semicolon` is a node that forces its input then discards it
  function(cont, ...) {
    force(cont)
    named(`;` <- function(val) {
      force(val)
      val <- NULL #force, then discard
      cont()
    })
    `;`
  }
}

`{_cps` <- function(.contextName, ...) {
  force(.contextName)
  args <- list(...)
  #args <- args[!(missing_(args))]
  function(cont, ...) {
    force(cont)
    if (length(args) == 0) {
      named(`{_` <- function() cont(NULL))
    } else if (length(args) == 1) {
      # just use the inner arg's continuation, like ()
      args[[1]](cont, ...)
    } else {
      # build a chain last to first linked with "semicolons"
      entries <- rep(list(NULL), length(args))
      entries[[length(args)]] <- args[[length(args)]](cont, ...)
      for (i in rev(seq_len(length(args) - 1))) {
        semi <- `;_ctor`(paste0(.contextName, i, ";"))(entries[[i+1]], ...)
        # each arg continues to a semicolon then its successor
        entries[[i]] <- args[[i]](semi, ...)
      }
      entry <- entries[[1]]
      entries <- NULL
      entry
    }
  }
}

# Here's how signals and restarts are handled: context constructors
# accept and pass down a list of handlers (...) through the right hand
# side. Of these we have "cont" and "bounce*" always. If a CPS function
# wants a signal "next" or such, it does that by calling into its
# `nxt` handler.

next_cps <- function(.contextName) {
  force(.contextName)
  function(cont, ..., nxt, trace=trace_) {
    if (missing_(arg(nxt))) stop("call to next is not in a loop")
    list(nxt, trace)
    if (options$verbose) {
      named(next_ <- function() {
        trace("next\n")
        nxt()
      })
    } else nxt
  }
}

break_cps <- function(.contextName) {
  force(.contextName)
  function(cont, ..., brk, trace=trace_) {
    if (missing_(arg(brk))) stop("call to break is not in a loop")
    list(cont, brk, trace)
    if(options$verbose) {
      named(break_ <- function() {
        trace("break\n")
        brk()
      })
    } else brk
  }
}

nextElemOr_cps <- function(.contextName, expr, or) {
  list(.contextName, expr, or)
  function(cont, ..., trace=trace_) {
    list(cont, trace_)

    or_ <- or(cont, ..., trace=trace_)

    named(getNext <- function(val) {
      stopping <- FALSE
      #when munged we had a name collision vs 'nextElemOr' in gen.r
      val <- async::nextElemOr(val, stopping <- TRUE)
      if (stopping) or_() else cont(val)
    })

    expr(getNext, ..., trace=trace_)
  }
}


# If you want to establish a new target for "break" or "next" you
# pass that down to constructor arguments:

repeat_cps <- function(.contextName, expr) { list(.contextName, expr)
  function(cont, ..., bounce, brk, nxt, trace=trace_) {
    list(cont, maybe(brk), maybe(nxt), trace)
    named(repeat_ <- function(val) {
      trace("repeat: again\n")
      force(val)
      val <- NULL
      bounce(begin)
    })
    named(brk_ <- function() {
      trace("repeat: break\n")
      cont(invisible(NULL))
    })
    named(nxt_ <- function() {
      trace("repeat: next\n")
      bounce(begin)
    })
    begin <- expr(repeat_, ..., bounce=bounce,
                  brk=brk_, nxt=nxt_, trace=trace)
    begin
  }
}

while_cps <- function(.contextName, cond, expr) {
  list(.contextName, cond, expr)
  function(cont, ..., bounce, bounce_val, nxt, brk, trace=trace_) {
    list(cont, bounce, bounce_val, maybe(nxt), maybe(brk), trace)
    named(again <- function(val) {
      force(val)
      val <- NULL
      bounce(begin)
    })
    named(brk_ <- function() {
      trace("while: break\n")
      bounce_val(cont, invisible(NULL))
    })
    named(nxt_ <- function() {
      trace("while: next\n")
      bounce(begin)
    })
    doExpr <- expr(again, ..., bounce=bounce, bounce_val=bounce_val,
                   nxt=nxt_, brk=brk_, trace=trace)
    named(while_ <- function(val) {
      if (val) {
        trace("while: do\n")
        val <- NULL
        doExpr()
      } else {
        trace("while: end\n")
        val <- NULL
        bounce_val(cont, invisible(NULL))
      }
    })
    begin <- cond(while_, ..., bounce=bounce, bounce_val=bounce_val,
                  nxt=nxt_, brk=brk_, trace=trace)
  }
}


for_cps <- function(.contextName, var, seq, expr) {
  list(.contextName, var, seq, expr)
  function(cont, ..., bounce, bounce_val, nxt, brk, awaitNext, sto, stp, trace=trace_) {
    list(cont, bounce, bounce_val, maybe(nxt), maybe(brk), maybe(awaitNext), trace)
    #quote the LHS at construction time
    var_ <- var(cont, ..., bounce=bounce, bounce_val=bounce_val,
                sto=sto, stp=stp, trace=trace, nxt=nxt,
                brk=brk, awaitNext=awaitNext) #not our brk/nxt
    if (!is_R(var_)) stop("Unexpected stuff in for() loop variable")
    var_ <- R_expr(var_)
    if (!is.name(var_)) stop("Expected a name in for() loop variable")
    var_ <- as.character(var_)
    seq_ <- NULL

    named(brk_ <- function() {
      cont(invisible(NULL))
    })

    if (is_missing(awaitNext)) {
      named(nxt_ <- function(val) {
        bounce(do_)
      })
    } else {
      is_async <- FALSE
      named(nxt_ <- function(val) { #hmm, may be called with 0 or 1 args, just ignores
        val <- NULL
        if (is_async) {
          await_()
        } else {
          bounce(do_)
        }
      })
    }

    body <- expr(nxt_, ..., bounce=bounce,
                 bounce_val=bounce_val, nxt=nxt_, brk=brk_,
                 sto=sto, stp=stp, trace=trace, awaitNext=awaitNext) # our brk_

    state <- "xxx"
    value <- NULL

    named(received <- function() {
      trace(paste0("for ", var_, ": received\n"))
      switch(state,
             "success"={state <<- "xxx"; sto(body, var_, value)},
             "error"={state <<- "xxx"; stp(value)},
             "closed"={state <<- "xxx"; cont(invisible(NULL))},
             stp(simpleError(paste0("awaitNext: unexpected state ", state))))
    })
    named(await_ <- function() {
      trace(paste0("for ", var_, ": awaiting\n"))
      awaitNext(received, seq_,
                function(val) {state <<- "success"; value <<- val},
                function(err) {state <<- "error"; value <<- err},
                function() {state <<- "closed"; value <<- NULL})
    })
    named(do_ <- function() {
      stopping <- FALSE
      trace(paste0("for ", var_, ": next\n"))
      val <- async::nextElemOr(seq_, stopping <- TRUE)
      if (stopping) {
        trace(paste0("for ", var_, ": finished\n"))
        cont(invisible(NULL))
      } else {
        trace(paste0("for ", var_, ": do\n"))
        sto(body, var_, val)
      }
    })
    if (is_missing(awaitNext)) {
      named(for_ <- function(val) {
        seq_ <<- async::iteror(val)
        do_()
      })
    } else {
      named(for_ <- function(val) {
        if (is.channel(val)) {
          is_async <<- TRUE
          seq_ <<- val
          await_()
        } else {
          seq_ <<- async::iteror(val)
          do_()
        }
      })
    }
    getSeq <- seq(for_, ..., bounce=bounce, bounce_val=bounce_val,
                  nxt=nxt, brk=brk, sto=sto, stp=stp, trace=trace, #not our brk
                  awaitNext=awaitNext)
  }
}


#' Asynchronous pause.
#'
#' "delay" returns a promise which resolves only after the specified
#' number of seconds. This uses the R event loop via [later].
#' In an `[async]` construct you can use `await(delay(secs))` to yield
#' control, for example if you need to poll in a loop.
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
