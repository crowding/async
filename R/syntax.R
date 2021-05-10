# Keywords which must always be CPS

# (or do we want to do something more focused in the parsing stage where
# we start with "yield" but on reaching a "while" we add "next" to the
# watchlist?
base_endpoints <- c("break", "next", "return")
async_endpoints <- c(base_endpoints, "await")
gen_endpoints <- c(base_endpoints, "yield")

# function names which block further translation of
# their arguments
base_blocks <- c("gen", "function", "async", "quote", "quo", "~", "alist",
                 "future", "future_promise")

# Translating an argument into CPS
cps_translate <- function(q, endpoints=base_endpoints, blocks=base_blocks,
                          local=TRUE, split_pipes=FALSE) {
  # The continuation constructors will be arranged in the same syntax
  # tree as the source expression, just about. The trick here is to redirect
  # the `for`, `while`, `if` onto their CPS counterparts.

  expr <- expr(q)
  target_env <- env(q)
  qualify <- function(name) {
    if (exists(as.character(name), envir=target_env)) {
      as.symbol(name)
    } else {
      call(":::", quote(async), as.symbol(name))
    }
  }

  arg_wrapper <- call(":::", quote(async), quote(R))

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
  #   direct R code in "R" if there's no other translation available.
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
    t_rest_cps <- as.logical(lapply(t_rest, function(x) x$cps))
    any_cps <- any(t_head$cps, t_rest_cps)
    if (any_cps) {
      promoted <- c(list(try_promote_head(t_head)),
                    lapply(t_rest, promote_arg))
      if (! promoted[[1]]$cps) {
        # we have an ordinary R function, but some argument wants to be pausable
        # Can we split?
        can_split_pipe <-
          isFALSE(t_head$cps) &&
          length(t_rest) >= 1 &&
          isTRUE(t_rest_cps[1]) &&
          (is.null(names(t_rest)) || names(t_rest)[1] == "") &&
          !any(t_rest_cps[-1])
        if (can_split_pipe) {
          if (split_pipes) {
            if ("alt" %in% names(t_rest[[1]])) {
              # continue a previous split
              preamble <- t_rest[[1]]$preamble
              t_rest[[1]] <- t_rest[[1]]$alt
            } else {
              # split out arg 1
              preamble <- as.call(list(qualify("<-_cps"),
                                       as.call(list(arg_wrapper,
                                                    quote(..async.tmp))),
                                       t_rest[[1]]$expr))
              t_rest[[1]]$expr <- quote(..async.tmp)
              t_rest[[1]]$cps <- FALSE
            }
            new_call <- as.call(lapply(c(list(t_head), t_rest),
                                       function(x)x$expr))
            list(expr=as.call(list(qualify("{_cps"),
                                   preamble,
                                   as.call(list(arg_wrapper,
                                                new_call)))),
                 cps=TRUE,
                 preamble=preamble,
                 alt=list(expr=new_call, cps=FALSE))
          } else {
            stop("A pause or break appears in an argument to `",
                 deparse(t_head$expr),
                 "`, which is not pausable. Consider using split_pipes=TRUE")
          }
        } else {
          stop("A pause or break appears in an argument to `",
               deparse(t_head$expr),
               "`, which is not pausable.")
        }
      } else {
        assert(all(as.logical(lapply(promoted, function(x) x$cps))),
                    msg=paste0("Could not make some arguments of",
                               " `", deparse(t_head$expr), "` pausable"))
        list(expr = as.call(lapply(promoted,
                                   function(x)x$expr)),
             cps = TRUE)
      }
    } else {
      # no pausables in this call so just wrap it
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
           `(` =,
           call = (is_qualified_name(expr) &&
                     as.character(expr[[3]]) %in% blocks),
           FALSE
           )
  }

  trans_head <- function(expr) {
    switch(mode(expr),
           `(` =,
           call = {
             if (is_qualified_name(expr)) {
               trans_qualified_head(expr)
             } else {
               t_call <- trans_call(expr)
               if (t_call$cps)
                 stop("Pauses / breaks not allowed in head position of call: ",
                      deparse(expr))
               t_call
             }
           },
           name =,
           character = {
             if (as.character(expr) %in% endpoints) {
               new_name <- try_promote_function_name(expr)
               assert(
                 new_name$cps,
                 msg=paste0(
                   deparse(expr),
                   " is listed as an endpoint, but can't find a pausable definition"))
               new_name
             } else {
               list(expr=expr, cps=FALSE)
             }
           },
           { #default. What did you do, call a constant?
             list(expr=expr, cps=FALSE)
           })
  }

  is_qualified_name <- function(expr) {
    is.call(expr) && is.symbol(expr[[1]]) && as.character(expr[[1]]) %in% c("::", ":::")
  }

  trans_qualified_head <- function(expr) {
    package <- expr[[2]]
    name <- expr[[3]]
    if (   as.character(package) %in% c("async", "base")
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

  try_promote_head <- function(l) {
    if (l$cps || blocked(l$expr)) {
      l
    } else {
      switch(mode(l$expr),
             name = {
               n <- try_promote_function_name(l$expr)
               if (n$cps)
                 n else {
                   l
                 }
             },
             `(`=,
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

  try_promote_function_name <- function(name) {
    try_promote_function_name_(name, target_env)
  }

  out <- trans(expr)
  if (! out$cps) {
    warning("no keywords (", paste(endpoints, collapse=", "), ") used?")
    out <- promote_arg(out)
  }
  if(local) {
    out$expr <- as.call(list(as.call(list(quote(`function`), NULL, out$expr))))
  }
  quo_(out$expr, target_env)
}



# Given a name, map it onto a CPS function if one is defined, and return
# it with the necessary namespace qualification.
#
# If you want to add a custom control flow operator: Say it's called
# "deflate_promise". Do the following: export a normal function named
# "deflate_promise" from your package, then implement a function
# named `deflate_promise_cps`. The second function need not be exported,
# find_cps_version will look in the package namespace for it.
try_promote_function_name_ <- function(name, target_env) {
  original_name <- as.symbol(name)
  potential_name <- as.symbol(paste0(as.character(name), "_cps"))
  where_found <- locate_(potential_name, target_env, mode="function", ifnotfound=NULL)
  if (!is.null(where_found)) {
    # A CPS function is defined in full view, excellent.
    list(expr=potential_name, cps=TRUE)
  } else {
    where_found <- locate_(original_name, target_env, mode="function", ifnotfound=NULL)
    if (is.null(where_found)) {
      # async::gen(... yield() ...) should find "yield" even if
      # async is not attached.
      where_found <- locate_(original_name,
                             getNamespace("async"),
                             ifnotfound=NULL)
      if (is.null(where_found)) {
        stop("Function `", as.character(original_name), "` was not found.")
      }
    }
    obj <- get(as.character(original_name), envir=where_found, mode="function")
    if (   is.primitive(obj)
        || identical(where_found, baseenv())
        || identical(environment(obj), getNamespace("base"))) {
      # look in async's exported namespace.
      if (exists(as.character(potential_name),
                 getNamespace("async"), inherits=FALSE)) {
        list(expr = call(":::", quote(async), potential_name),
             cps = TRUE)
      } else {
        list(expr=original_name, cps=FALSE)
      }
    } else {
      # look alongside found function
      lookin <- environment(obj)
      if (exists(as.character(potential_name),
                 lookin, inherits=FALSE)) {
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
        #guess it's an ordinary function
        list(expr = original_name, cps = FALSE)
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
    if (as.character(package) == "base") package <- quote(async)
    new_name <- as.symbol(paste0(as.character(name), "_cps"))
    loadNamespace(package)
    if (exists(new_name, .getNamespace(package))) {
      list(expr=call(":::", package, new_name),cps=TRUE)
    } else list(expr=l$expr, cps=FALSE)
  }
}


#' Pausable functions.
#'
#' [`async`] and [`gen`] rely on "pausable" workalikes for R functions
#' like `if`, `while`, and so on. `pausables()` scans for and returns
#' a list of all pausable functions visible in the present environment
#' and in attached packages.
#'
#' It is possible for a third party package to define pausable
#' functions. To do this:
#
#' 1. Define and export a function `yourname` and an ordinary R implementation
#' (the pausable version is only used when there is an `await` or
#' `yield` in the arguments.)
#' 2. Also define a function `yourname_cps` in your package namespace. (It
#' does not need to be exported.) `yourname_cps` should have the pausable
#' (callback based)
#' implementation.
#'
#' The API for pausable functions is not yet fixed, but it is described
#' in source file `cps.r` along with implementataions for core R functions.
#'
#' @param envir The environment to search (defaulting to the calling
#'   environment).
#' @param packages Which packages to search; defaults to currently
#'   loaded packages. You can scan all packages with
#'   `pausables(packages=base::.packages(all.available=TRUE))`
#' @return A list of expressions (either names or `:::` calls)
#' @export
pausables <- function(envir=caller(),
                      packages=base::.packages()) {
  visible_pausables <- visible_names(envir) |>
  lapply(function(name) {
    name <- as.symbol(name)
    tryCatch(c(list(orig=name), try_promote_function_name_(name, envir)),
             error=function(e) list(orig=name,err=e, expr=name, cps=FALSE))
  })   |>
  Filter(f=function(x) x$cps) |>
  lapply(function(x) x$orig)

  qualified_names <- packages |>
  lapply(function(p) tryCatch(
    lapply(getNamespaceExports(p),
           function(x) {
             call("::", as.name(p), as.name(x))
           }),
    error=function(err) {warning(err); list()})) |>
  unlist() |>
  unique()

  qualified_pausables <- qualified_names |>
  lapply(function(x)
    c(list(orig=x), promote_qualified_head(list(expr=x, cps=FALSE)))) |>
  Filter(f=function(x)x$cps) |>
  Map(f=function(x)x$orig)

  c(visible_pausables, qualified_pausables)
}

visible_names <- function(envir=caller()) {
  if (is.null(envir) || identical(envir, emptyenv()))
    c()
  else
    union(ls(envir), visible_names(parent.env(envir)))
}
