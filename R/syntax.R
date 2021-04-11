# Keywords which must always be CPS

# (or do we want to do something more focused in the parsing stage where
# we start with "yield" but on reaching a "while" we add "next" to the
# watchlist?
base_endpoints <- c("break", "next")
async_endpoints <- c(base_endpoints, "await")
gen_endpoints <- c(base_endpoints, "yield")

# function names which block further translation of
# their arguments
base_blocks <- c("gen", "function", "async", "quote", "quo")

# Translating an argument into CPS
cps_translate <- function(q, endpoints=base_endpoints, blocks=base_blocks) {
  # The continuation constructors will be arranged in the same syntax
  # tree as the source expression, just about. The trick here is to redirect
  # the `for`, `while`, `if` onto their CPS counterparts.

  expr <- expr(q)
  target_env <- env(q)
  qualify <- function(name) {
    if (exists(as.character(name), envir=target_env)) {
      as.symbol(name)
    } else {
      call(":::", quote(generators), as.symbol(name))
    }
  }

  arg_wrapper <- qualify("arg_cps")

  wrap_arg <- function(l) {
    if (l$cps) {
      l$expr
    } else {
      call(qualify("arg_cps"), l$expr)
    }
  }

  # The following functions deal in this datatype: list(expr=quote(...),
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
    ## This should be made to do something sensible with namespaces?
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
                      " `", deparse(expr),
                      "` but it is listed as a control operator.")
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
      list(expr = as.call(list(arg_wrapper, l$expr)), cps=TRUE)
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
                 n else {
                   stop("Could not find a CPS implementation of",
                        " `", deparse(l$expr), "`.")
                 }
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
    where_found <- locate_(potential_name, target_env, mode="function", ifnotfound=NULL)
    if (!is.null(where_found)) {
      # A CPS function is defined in full view, excellent.
      list(expr=potential_name, cps=TRUE)
    } else {
      where_found <- locate_(original_name, target_env, ifnotfound=NULL)
      if (is.null(where_found)) {
        # generators::gen(... yield() ...) should find "yield" even if
        # generators is not attached
        where_found <- locate_(original_name,
                               getNamespace("generators"),
                               ifnotfound=NULL)
        if (is.null(where_found)) {
          stop("Function `", as.character(original_name), "` was not found.")
        }
      }
      obj <- get(as.character(original_name), envir=where_found, mode="function")
      if (   is.primitive(obj)
          || identical(where_found, baseenv())
          || identical(environment(obj), getNamespace("base"))) {
        # look in generators' exported namespace.
        if (exists(as.character(potential_name),
                   getNamespace("generators"), inherit=FALSE)) {
          list(expr = call(":::", quote(generators), potential_name),
               cps = TRUE)
        } else {
          list(expr=original_name, cps=FALSE)
        }
      } else {
        if (is.function(obj)) {
          # look alongside found function
          lookin <- environment(obj)
          if (exists(as.character(potential_name),
                     lookin, inherit=FALSE)) {
            if(isNamespace(lookin)) {
              list(expr = call(":::",
                               as.symbol(getNamespaceName(lookin)),
                               as.symbol(potential_name)),
                   cps = TRUE)
            } else {
              # ugh just include the literal function
              list(expr = get(as.character(potential_name), environment(obj)),
                   cps = TRUE)
            }
          } else {
            stop("No candidate found for `", as.character(potential_name), "`.")
          }
        } else {
          stop("Variable `", deparse(expr), "` does not contain a function.")
        }
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
