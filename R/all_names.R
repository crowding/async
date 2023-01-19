
# Scan a function and return all names used, labeled by their
# (possibly overlapping) "roles":
#
# - arg: names defined as arguments
# - var: names appearing in arguments of calls
# - call: names appearing at head of calls
# - local: names appearing as target of <-
# - store: names appearing as target of <<-
# - tail: names of calls in "tail position"
# - hand: names that refer to functions(cont, ...) that are called with functions
#         (i.e. trampoline handlers)
# - tramp: names that are passed to "cont" of a trampoline handler
# - utility: names of NON-tailcalls whose targets are bound in
#            the same function's environment.
# - tailcall: the entire calls in tail position.
#             (a list, possibly with original form for trampolines)
# - trampoline: entire trampolined calls
#             (also a list with orginal forms)
# - handler: the trampoline handler takes the continuation
#             (also a list with original forms)
all_names <- function(fn, nonTail=TRUE, forGraph=FALSE) {
  collect(type=if(forGraph) list() else character(0),
          function(yield)
            visit_function(fn, yield, nonTail=nonTail, forGraph=forGraph))
}

visit_function <- function(fn, yield, nonTail=TRUE, forGraph=FALSE) {
  env <- environment(fn)
  locals <- character(0)
  yield_ <- function(val, name) {
    if(forGraph) switch(
      name,
      arg=,
      local=
        locals <<- c(locals, val),
      trampoline=,
      handler=,
      tailcall=,
      windup= {
        nm <- as.character(val[[1]][[1]])
        if (!(nm %in% locals))
          if (exists(nm, env, inherits=FALSE))
            yield(val, name=name)
      },
      local= locals <<- c(locals, val),
      NULL
    )
    else switch(
      name,
      trampoline=,
      handler=,
      tailcall=,
      windup= NULL,
      tramp=,
      hand=,
      wind=,
      tail= if (!val %in% locals) {
        if (exists(val, env, inherits=FALSE))
          yield(val, name=name)
        else yield(val, name="call")
      },
      var= if (!val %in% locals) {
        if (exists(val, env, inherits=FALSE))
          yield(val, name="read")
        else yield(val, name=name)
      },
      arg=,
      local= {
        locals <<- c(locals, val)
        yield(val, name=name)
      },
      call= if (!val %in% locals) {
        if (exists(val, env, inherits=FALSE)) {
          if (!is.na(has_global_name(val))) {
            #this happens in gen:nextElemOr when it calls "pump"
            #don't treat as a utility function.
            yield(val, name="call")
          } else yield(val, name="util")
        } else yield(val, name=name)
      },
      wind= if (!val %in% locals) {
        if (exists(val, env, inherits=FALSE))
          yield(val, name="wind")
        else yield(val, name="call")
      },
      store= if (exists(val, env, inherits=FALSE)) {
        yield(val, name="store")
      } else stop(paste0("Target of <<- not found: ", val)),
      stop(paste0("Unknown argument type", name))
    )
  }
  visit_lambda <- function(expr, inTail, orig=NULL, yield) {
    locals <- names(expr[[2]])
    yield_ <- function(val, name) {
      switch(name,
             call= if (!val %in% locals) {
               yield(val, name)
             },
             var= if (!val %in% locals) {
               yield(val, "var")
             },
             arg=,
             local= locals <<- c(locals, val),
             tail=,
             store=,
             yield(val, name)
             )
    }
    visit_arg(expr[[3]], inTail=inTail, yield=yield_)
  }
  visit_head <- function(expr, inTail, yield) {
    if (!inTail && !nonTail) return()
    switch(mode(expr),
           call={
             switch(
               mode(expr[[1]]),
               name=, character= {
                 # could be a "::" or ":::"
                 if (as.character(expr[[1]]) %in% c("::", ":::")) {
                   name <- paste0(as.character(expr[[2]]),
                                  as.character(expr[[1]]),
                                  as.character(expr[[3]]))
                   if(inTail) yield(name, name="tail")
                   else yield(name, name="call")
                 } else {
                   visit_call(expr, inTail=FALSE, yield=yield)
                 }
               },
               visit_call(expr, inTail=FALSE, yield=yield)
             )
           },
           character=,
           name={
             if(inTail) {
               yield(as.character(expr), "tail")
             } else yield(as.character(expr), "call")
           },
           NULL
           )
  }
  visit_weird_call <- function(expr, inTail, orig=NULL, yield) {
    # i.e. trampolines and handlers don't need to register as tailcalls
    # we've already visited the head.
    if(nonTail) {
      for (i in unname(as.list(expr)[-1])) {
        if (!missing(i))
          visit_arg(i, inTail=inTail, yield=yield)
      }
    }
  }
  visit_ordinary_call <- function(expr, inTail, orig=NULL, yield) {
    if (inTail) yield(c(list(expr), orig), "tailcall")
    visit_head(expr[[1]], inTail=inTail, yield=yield)
    visit_weird_call(expr, inTail, orig, yield)
  }
  visit_call <- function(expr, inTail, orig=NULL, yield) {
    if (!inTail && !nonTail) return(character(0))
    head <- expr[[1]]
    switch(mode(head),
           character=,
           name={
             head <- as.character(head)
             if (head %in% locals) {
               visit_weird_call(expr, inTail, NULL, yield)
             } else if (exists(head, env, inherits=FALSE)) {
               peek <- get0(head, envir=env, ifnotfound=NULL, inherits=FALSE)
               if (!all(c("cont") %in% names(formals(peek)))) {
                 visit_ordinary_call(expr, inTail, orig, yield)
               } else {
                 # A trampoline-indirect call! Register both the
                 # target and the indirect.
                 handl <- match.call(peek, expr, expand.dots=FALSE,
                                     envir=as.environment(list(`...`=NULL)))
                 if ("winding" %in% names(handl)) {
                   # windup takes TWO function pointers
                   woundup <- as.call(list(handl$winding))
                   windup <- TRUE
                   handl$winding <- NULL
                   yield(as.character(woundup[[1]]), "wind")
                   yield(c(list(woundup, expr), orig), "windup")
                   visit_weird_call(woundup, inTail, c(list(expr), orig), yield)}
                 trampoline_args <- names(handl) %in% c("cont", "val")
                 trampolined <- as.call(handl[trampoline_args])
                 handl <- handl[!trampoline_args]
                 yield(as.character(trampolined[[1]]), "tramp")
                 yield(as.character(expr[[1]]), "hand")
                 yield(c(list(handl, expr), orig), "handler")
                 yield(c(list(trampolined, expr), orig), "trampoline")
                 visit_weird_call(handl, FALSE, c(list(expr), orig), yield)
               }
             } else {
               # something that isn't bound in the immediately
               # enclosing environment.
               switch(
                 head,
                 "=" =, "<-" = {
                   visit_arg(expr[[3]], inTail=FALSE, yield)
                   visit_store(expr[[2]], "local", yield)},
                 "<<-" = {
                   visit_arg(expr[[3]], inTail=FALSE, yield)
                   visit_store(expr[[2]], "store", yield)},
                 "if" = {
                   visit_arg(expr[[2]], inTail=FALSE, yield)
                   visit_arg(expr[[3]], inTail=inTail, yield)
                   if (length(expr) >= 4)
                     visit_arg(expr[[4]], inTail=inTail, yield)},
                 # the argument to return() is not considered to be
                 # in the tail because it won't be a safe place to splice
                 "return" = visit_arg(expr[[2]], inTail=FALSE, yield),
                 "while" = {
                   visit_arg(expr[[2]], inTail=FALSE, yield)
                   visit_arg(expr[[3]], inTail=FALSE, yield)},
                 "for" = {
                   visit_store(expr[[2]], "local", yield)
                   visit_arg(expr[[3]], inTail=FALSE, yield)
                   visit_arg(expr[[4]], inTail=FALSE, yield)},
                 "("= visit_arg(expr[[2]], inTail=inTail, yield),
                 "{"= {
                   if(nonTail && length(expr) > 2) {
                     for (i in 1+seq_len(length(expr)-2))
                       visit_arg(expr[[i]], inTail=FALSE, yield)}
                   if (length(expr) > 1)
                     visit_arg(expr[[length(expr)]], inTail=inTail, yield)},
                 "||"=,"&&"={
                   visit_arg(expr[[2]], inTail=FALSE, yield)
                   visit_arg(expr[[3]], inTail=inTail, yield)},
                 "switch"={
                   visit_arg(expr[[2]], inTail=FALSE, yield)
                   if(nonTail) {
                     for (i in 2+seq_len(length(expr)-2)) {
                       visit_arg(expr[[i]], inTail=inTail, yield)}}},
                 "tryCatch"={
                   visit_arg(expr[[2]], inTail=inTail, yield)
                   visit_arg(expr[[3]], inTail=inTail, yield)},
                 "function"={
                   # A lambda passed into a tail (e.g. tryCatch in tail position)
                   # might indeed be a tailcall
                   visit_lambda(expr, inTail=TRUE, NULL, yield)},
                 "on.exit"={
                   expr <- match.call(
                     function (expr = NULL, add = FALSE, after = TRUE) NULL,
                     expr)
                   # this ought to be visiting at top level though?
                   visit_arg(expr$expr, inTail=TRUE, yield)
                   expr$expr <- NULL
                   for (i in seq_len(length(expr)-1)+1) {
                     visit_arg(expr$expr[[i]], inTail=FALSE, yield)
                   }
                 },
                 {
                   #all other named calls not bound here
                   visit_ordinary_call(expr, inTail, orig, yield)
                 }
               )
             }
           },
           # something other than a name in a call head
           if(nonTail) {
             visit_head(expr[[1]], FALSE, yield)
             for (i in as.list(expr)[-1]) visit_arg(i, inTail, yield)
           }
           )
  }
  visit_arg <- function(expr, inTail, yield) {
    if (!inTail && !nonTail) return(NULL)
    switch(mode(expr),
           call=visit_call(expr, inTail, NULL, yield),
           name=if(!missing_(expr)) {
             yield(as.character(expr), "var")
           })
  }
  visit_store <- function(dest, how, yield) {
    switch(mode(dest),
           call={
             # In a complex assignment like foo[bar] <- baz;
             # or equivalently `[`(foo, bar) <- baz;
             # R effectively expands it to:
             # foo <- `[<-`(foo, baz, bar)
             # which means "foo" should count as both "local"
             # and "var", and we should count a call to "[<-".
             visit_store(dest[[2]], how, yield)
             visit_call(
               as.call(c(list(as.name(paste0(dest[[1]], "<-"))),
                         as.list(dest)[-1])),
               inTail=FALSE, orig=NULL, yield=yield)
           },
           character=,
           name=
             yield(as.character(dest), how)
           )
  }
  for (i in names(formals(fn))) yield_(i, "arg")
  visit_arg(body(fn), inTail=TRUE, yield=yield_)
}
