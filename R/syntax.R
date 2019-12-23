#' these are function names which block further translation of
#' their arguments
base_blocks <- c("gen", "function", "delay", "quote")

# Base keywords which flip the CPS switch
# (or do we want to do something more focused in the parsing stage where
# we start with "yield" but on reaching a "while" we add "next" to the
# watchlist?
base_endpoints <- c("break", "next")
yield_endpoints <- c(base_endpoints, "yield")
delay_endpoints <- c(base_endpoints, "block", "resolve")

#' Create an iterator using sequential code.
#'
#' `gen({...})` with an expression written in its argument, creates a
#' generator, which can be thought of as a block of code whose
#' execution can pause and resume. From the inside, a generator looks
#' like you are writing sequential code with loops, branches and such,
#' writing values to the outside world by calling `yield()`. From the
#' outside, a generator behaves like an iterator over
#' an indefinite collection.
#'
#' Generators are not based on forking or parallel OS processes; they
#' run in the same thread as their caller. The control flow in a
#' generator is interleaved with that of the R code which queries it.
#'
#' When `nextItem` is called on a generator, the generator evaluates
#' its given expression until it reaches a call to `yield(...).` The
#' value passed to `yield` is returned. The generator's execution
#' state is preserved and will continue form where ti left off on the
#' next call to `nextItem.`
#'
#' There are some syntactic restrictions on what you can write in a
#' generator expression. Wherever `yield` appears in a generator
#' expression, the calls it is nested within must have CPS
#' implementations. (This package provides CPS implementations for
#' several base R control flow builtins; the list is in the non-exported
#' variable `generators:::cps_builtins`).
#' @export
gen <- function(expr, ...) {
  do(make_generator,
     cps_translate(arg(expr),
                   endpoints=yield_endpoints),
     dots(...))
}

# Create an asynchronous task from sequential code.
#
# `delay({...})`, with an expression written in its argument, allows
# that expression to be evaluated in an asynchronous, or non-blocking
# manner. `delay` returns an object with class c("delay", "future",
# "promise") which implements both the [promise] and "future"
# interfaces.
#
# When a `delay` object is activated, it will evaluate its expression
# until it reaches the keyword `block`. The delay object will return
# to its caller and preserve the partial state of its evaluation.
# When the delay is next activated, evaluation continues from where
# it last left off. Usually you would use `block` to wait until an
# external condition is is satisfied, like
# `while(!messageAvailable(conn)) block`.
#
# When a delay completes evaluation of its expression, the promise
# resolves with the resulting value. If an error is raised from the
# delay expression, the promise rejects and stores that error.
#
# Note that resolution of a delay, while asynchronous, happens in the
# same thread which requests the value -- there is no forking or
# parallel processing involved.  It is a bit more like cooperative
# multitasking, where `delay` pauses the current task and
# switches to another one.
#
# This means that delay objects do not execute their code when they
# are not being "observed." If you feed a delay object to
# futures:::resolve, `resolve` will poll it until it resolves, like
# with other kinds of futures. It is this polling which prods the delay
# to run until its next "block()"
#
# Normally a [promises::promise] object can not be directly
# unwrapped, and the value is only received by callback. But within a
# delay expression, you can call "resolve" directly on a promise (or
# a future.) From the standpoint inside the delay, this will block
# until it returns the value the promise resolves to. From the standpoint
# outside the delay object, it means the delay object returns
# immediately, and does not finish evaluating until the promise it
# has queried finishes.
#
# The syntactic rules for a delay are analogous to that for [gen()];
# wherever a 'block' appears, its surrounding syntax must have a CPS
# implementation available. Thus the following delay:
#
# `delay({
#   readMessage(conn, nonblocking=TRUE)
#   while(!receivedMessage(conn))
#    if (alive(conn) block else stop(lastError(conn)))
# })`
#
# requires CPS implementations for `while`, `{` and `if`. CPS
# implementations for these and most base R control flow constructs
# are provided in this package, for instance the non-exported object
# `generators:::if_cps`.
#
# @param expr An expression, to be evaluated asynchronously on demand.
# @return A [promises::promise] object.
delay <- function(expr, ...) {
  do(make_delay,
     translate_cps(arg(expr), delay_endpoints),
     dots(...))
  expr <- arg(expr)
  expr$resolve()
}

base_endpoints <- c("break", "next")

# Translating an argument into CPS
cps_translate <- function(q, endpoints=base_endpoints, blocks=base_blocks) {
  # The continuation constructors will be arranged in the same syntax
  # tree as the source expression, just about. The trick here is to redirect
  # the `for`, `while`, `if` onto their CPS counterparts.

  expr <- expr(q)
  target_env <- env(q)

  qualify <- function(name) {
    name
  }

  arg_wrapper <- qualify("arg_cps")

  wrap_arg <- function(l) {
    if (l$cps) {
      l$expr
    } else {
      call(qualify("arg_cps"), l$expr)
    }
  }

  # Subsequent functions deal in this datatype: list(expr=quote(...),
  #  cps=logical) with field cps being TRUE if CPS translation has been done
  #  on the subexpression.
  #
  # Outline of the recursive algorithm:
  #
  # The "trans" functions recursively scan the expression and
  #   translate anything like `yield` which _must_ be in cps style.
  #   trans(expr) only does translation and returns cps=TRUE it it found
  #   an endpoint (i.e. "yield")
  # The "promote" functions try harder to put things into CPS style, wrapping
  #   direct R code in "arg_cps" if there's no other translation available.
  #   They only return a result with cps=FALSE if the argument could not be
  #   translated.
  #
  # trans_call() recursively invokes trans() on the call arguments,
  # and if any are returned with cps=true, the rest of the
  # arguments are given to promote_arg() to wrap them, and the head of
  # the call is changed to a CPS equivalent.

  trans <- function(expr) {
    # returns a list(expr=, cps=boolean())
    switch(mode(expr),
           `(`=,
           call=trans_call(expr),
           list(expr=expr, cps=FALSE)
           )
  }

  trans_call <- function(expr) {
    if (blocked(expr[[1]])) {
      return(list(expr=expr, cps=FALSE))
    }
    t_rest <- lapply(expr[-1], trans)
    t_head <- trans_head(expr[[1]])
    needs_cps <- any(t_head$cps,
                     as.logical(lapply(t_rest,
                                       function(x) x$cps)))
    if (needs_cps) {
      promoted <- c(list(promote_head(t_head)),
                    lapply(t_rest, promote_arg))
      if (! promoted[[1]]$cps)
        stop("Could not find a CPS implementation for",
             " `", deparse(t_head$expr), "`.")
      if (! all(as.logical(lapply(promoted, function(x) x$cps))))
        stop("Could not make some arguments of",
             " `", deparse(t_head$expr), "` into CPS form")
      list(expr = as.call(lapply(promoted,
                                 function(x)x$expr)),
           cps = TRUE)
    } else {
      list(expr =
             as.call(lapply(c(list(t_head), t_rest),
                            function(x)x$expr)),
           cps = FALSE)
    }
  }

  blocked <- function(expr) {
    ## This should be made to do something sensible with namespaces
    switch(mode(expr),
           character = ,
           name = as.character(expr) %in% blocks,
           call = (is_qualified_name(expr) &&
                     as.character(expr[[3]]) %in% blocks),
           FALSE
           )
  }

  trans_head <- function(expr) {
    switch(mode(expr),
           call = {
             if (is_qualified_name(expr)) {
               trans_qualified_head(expr)
             } else {
               t_call <- trans_call(expr)
               if (t_call$cps)
                 stop("Control keywords not allowed in head position of call: ",
                      deparse(expr))
               t_call
             }
           },
           name =,
           character = {
             if (as.character(expr) %in% endpoints) {
               new_name <- promote_function_name(expr)
               if (new_name$cps) {
                 new_name
               } else {
                 stop("Could not find a CPS implementation of",
                      " `", deparse(l$expr),
                      "` but it is listed as a control operator")
               }
             } else {
               list(expr=expr, cps=FALSE)
             }
           })
  }

  is_qualified_name <- function(expr) {
    is.call(expr) && is.symbol(expr[[1]]) && as.character(expr[[1]]) %in% c("::", ":::")
  }

  trans_qualified_head <- function(expr) {
    package <- expr[[2]]
    name <- expr[[3]]
    if (   as.character(package) %in% c("generators", "base")
        && as.character(name) %in% endpoints) {
      promote_qualified_head(list(expr=expr, cps=FALSE))
    } else
      list(expr=expr, cps=FALSE)
  }

  # promote_ functions take in a list(expr=quote(), cps=logical(1))
  promote_arg <- function(l) {
    if(! l$cps) {
      list(expr = call(arg_wrapper, l$expr), cps=TRUE)
    } else {
      l
    }
  }

  promote_head <- function(l) {
    if (l$cps || blocked(l$expr)) {
      l
    } else {
      switch(mode(l$expr),
             name = {
               n <- promote_function_name(l$expr)
               if (n$cps)
                 n else stop("Could not find a CPS implementation of",
                             " `", deparse(l$expr), "`.")
             },
             call = {
               if (is_qualified_name(l$expr)) {
                 promote_qualified_head(l)
               } else {
                 l
               }
             },
             l
             )
    }
  }

  # Given a name, map it onto a CPS function if one is defined, and return
  # it with the necessary namespace qualification.
  #
  # If you want to add a custom control flow operator: Say it's called
  # "deflate_promise". Do the following: export a normal function named
  # "deflate_promise" from your package, then implement a function
  # named `deflate_promise_cps`. The second function need not be exported,
  # find_cps_version will look in the package namespace for it.
  promote_function_name <- function(name) {
    original_name <- as.symbol(name)
    potential_name <- as.symbol(paste0(as.character(name), "_cps"))
    found <- locate_(potential_name, target_env, mode="function", ifnotfound=NULL)
    if (!is.null(found)) {
      # A CPS function is defined in full view, excellent.
      list(expr=potential_name, cps=TRUE)
    } else {
      where_found <- locate_(original_name, target_env, ifnotfound=NULL)
      obj <- get(as.character(original_name), envir=target_env, mode="function")
      if (is.null(where_found)) {
        stop("Function `", deparse(cps), "` was not found.")
      }
      if (   is.primitive(obj)
          || identical(where_found, baseenv())
          || identical(environment(obj), getNamespace("base"))) {
        # look in generators' private namespace.
        if (exists(as.character(potential_name),
                   getNamespace("generators"), inherit=FALSE)) {
          list(expr=call(":::", name, quote(generators)),
               cps=TRUE)
        } else {
          list(expr=original_name, cps=FALSE)
        }
      } else {
        #look alongside object?
        list(expr=original_name, cps=FALSE)
      }
    }
  }

  promote_qualified_head <- function(l) {
    if (l$cps) {
      l
    } else {
      package <- l$expr[[2]]
      name <- l$expr[[3]]
      if (as.character(package) == "base") package <- quote(generators)
      new_name <- as.symbol(paste0(as.character(name), "_cps"))
      # at this point just trust the caller and stick a _cps on without
      # confirming its existence?
      ## if (exists(name, packageNamespace(as.character(new_name)))) {
      list(expr=call(":::", package, new_name),cps=TRUE)
      ## } else list(expr=expr, cps=FALSE)
    }
  }

  out <- trans(expr)
  if (! out$cps) {
    warning("no keywords (", paste(endpoints, collapse=", "), ") used?")
    out <- promote_arg(out)
  }
  quo_(out$expr, target_env)
}
