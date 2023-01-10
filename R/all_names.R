collect_tree <- function(fn) {
  size <- 32
  ilevel <- 1
  thislevel <- list(length=64)
  levels <- rep(list(size, length=size))
  iix <- 1

  append <- function(name, val, type) {
    
  }
  open <- function() {

  }
  close(type) <- function() {

  }
  fn(append, open, close)
}

collect <- function(fn, type) {
  size <- 64
  a <- vector(mode(type), length=size)
  i <- 0
  fn(function(name, val) {
    i <<- i + 1
    if (i >= size) {
      size <<- min(2 * size, i)
      length(a) <<- size
    }
    names(a)[[i]] <<- name
    a[[i]] <<- val
  })
  length(a) <- i
  a
}

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
            collect_function(fn, yield, nonTail=nonTail, forGraph=forGraph))
}

collect_function <- function(fn, yield, nonTail=TRUE, forGraph=FALSE) {
  env <- environment(fn)
  locals <- names(formals(fn)) %||% character(0)
  yield_ <- function(type, name) {
    if(forGraph) switch(
      type,
      trampoline=,
      handler=,
      tailcall=,
      windup= {
        nm <- as.character(name[[1]][[1]])
        if (!(nm %in% locals))
          if (exists(nm, env, inherits=FALSE))
            yield(type, name)
      },
      local= locals <<- c(locals, name),
      NULL
    )
    else switch(
      type,
      trampoline=,
      handler=,
      tailcall=,
      windup= NULL,
      tramp=,
      hand=,
      wind=,
      tail= if (!name %in% locals) {
        if (exists(name, env, inherits=FALSE))
          yield(type, name)
        else yield("call", name)
      },
      var= if (!name %in% locals) {
        if (exists(name, env, inherits=FALSE))
          yield("read", name)
        else yield(type, name)
      },
      local= {
        locals <<- c(locals, name)
        yield(type, name)
      },
      call= if (!name %in% locals) {
        if (exists(name, env, inherits=FALSE))
          yield("util", name)
        else yield(type, name)
      },
      wind= if (!name %in% locals) {
        if (exists(name, env, inherits=FALSE))
          yield("wind", name)
        else yield("call", name)
      },
      store= if (exists(name, env, inherits=FALSE)) {
        yield("store", name)
      } else stop(paste0("Target of <<- not found: ", name)),
      yield(type, name)
    )
  }
  collect_lambda <- function(expr, inTail, orig=NULL, yield) {
    locals <- names(expr[[2]])
    yield_ <- function(type, name) {
      switch(type,
             call= if (!name %in% locals) {
               yield(type, name)
             },
             var= if (!name %in% locals) {
               yield("var", name)
             },
             arg=,
             local= locals <<- c(locals, name),
             tail=,
             store=,
             yield(type, name)
             )
    }
    collect_arg(expr[[3]], inTail=inTail, yield=yield_)
  }
  collect_head <- function(expr, inTail, yield) {
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
                   if(inTail) yield("tail", name)
                   else yield("call", name)
                 } else {
                   collect_call(expr, inTail=FALSE, yield=yield)
                 }
               },
               collect_call(expr, inTail=FALSE, yield=yield)
             )
           },
           character=,
           name={
             if(inTail) {
               yield("tail", as.character(expr))
             } else yield("call", as.character(expr))
           },
           NULL
           )
  }
  collect_weird_call <- function(expr, inTail, orig=NULL, yield) {
    # i.e. trampolines and handlers don't need to register as tailcalls
    # we've already collected the head.
    if(nonTail) {
      for (i in unname(as.list(expr)[-1])) {
        if (!missing(i))
          collect_arg(i, inTail=inTail, yield=yield)
      }
    }
  }
  collect_ordinary_call <- function(expr, inTail, orig=NULL, yield) {
    if (inTail) yield("tailcall", c(list(expr), orig))
    collect_head(expr[[1]], inTail=inTail, yield=yield)
    collect_weird_call(expr, inTail, orig, yield)
  }
  collect_call <- function(expr, inTail, orig=NULL, yield) {
    if (!inTail && !nonTail) return(character(0))
    head <- expr[[1]]
    switch(mode(head),
           character=,
           name={
             head <- as.character(head)
             if (head %in% locals) {
               collect_weird_call(expr, inTail, NULL, yield)
             } else if (exists(head, env, inherits=FALSE)) {
               peek <- get0(head, envir=env, ifnotfound=NULL, inherits=FALSE)
               if (!all(c("cont") %in% names(formals(peek)))) {
                 collect_ordinary_call(expr, inTail, orig, yield)
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
                   yield("wind", as.character(woundup[[1]]))
                   yield("windup", c(list(woundup, expr), orig))
                   collect_weird_call(woundup, inTail, c(list(expr), orig), yield)}
                 trampoline_args <- names(handl) %in% c("cont", "val")
                 trampolined <- as.call(handl[trampoline_args])
                 handl <- handl[!trampoline_args]
                 yield("tramp", as.character(trampolined[[1]]))
                 yield("hand", as.character(expr[[1]]))
                 yield("handler", c(list(handl, expr), orig))
                 yield("trampoline", c(list(trampolined, expr), orig))
                 collect_weird_call(handl, FALSE, c(list(expr), orig), yield)
               }
             } else {
               # something that isn't bound in the immediately
               # enclosing environment.
               switch(
                 head,
                 "=" =, "<-" = {
                   collect_arg(expr[[3]], inTail=FALSE, yield)
                   collect_store(expr[[2]], "local", yield)},
                 "<<-" = {
                   collect_arg(expr[[3]], inTail=FALSE, yield)
                   collect_store(expr[[2]], "store", yield)},
                 "if" = {
                   collect_arg(expr[[2]], inTail=FALSE, yield)
                   collect_arg(expr[[3]], inTail=inTail, yield)
                   if (length(expr) >= 4)
                     collect_arg(expr[[4]], inTail=inTail, yield)},
                 # the argument to return() is not considered to be
                 # in the tail because it won't be a safe place to splice
                 "return" = collect_arg(expr[[2]], inTail=FALSE, yield),
                 "while" = {
                   collect_arg(expr[[2]], inTail=FALSE, yield)
                   collect_arg(expr[[3]], inTail=FALSE, yield)},
                 "for" = {
                   collect_store(expr[[2]], "local", yield)
                   collect_arg(expr[[3]], inTail=FALSE, yield)
                   collect_arg(expr[[4]], inTail=FALSE, yield)},
                 "("= collect_arg(expr[[2]], inTail=inTail, yield),
                 "{"= {
                   if(nonTail) {
                     for (i in 1+seq_len(length(expr)-2))
                       collect_arg(expr[[i]], inTail=FALSE, yield)
                   }
                   collect_arg(expr[[length(expr)]], inTail=inTail, yield)},
                 "||"=,"&&"={
                   collect_arg(expr[[2]], inTail=FALSE, yield)
                   collect_arg(expr[[3]], inTail=inTail, yield)},
                 "switch"={
                   collect_arg(expr[[2]], inTail=FALSE, yield)
                   if(nonTail) {
                     for (i in 2+seq_len(length(expr)-2)) {
                       collect_arg(expr[[i]], inTail=inTail, yield)}}
                 },
                 "tryCatch"={
                   collect_arg(expr[[2]], inTail=inTail, yield)
                   collect_arg(expr[[3]], inTail=inTail, yield)},
                 "function"={
                   # A lambda passed into a tail (e.g. tryCatch in tail position)
                   # might indeed be a tailcall
                   collect_lambda(expr, inTail=TRUE, NULL, yield)},
                 {
                   #all other named calls not bound here
                   collect_ordinary_call(expr, inTail, orig, yield)}
               )
             }
           },
           # something other than a name in a call head
           if(nonTail) {
             collect_head(expr[[1]], FALSE, yield)
             for (i in as.list(expr)[-1]) collect_arg(i, inTail, yield)
           }
           )
  }
  collect_arg <- function(expr, inTail, yield) {
    if (!inTail && !nonTail) return(NULL)
    switch(mode(expr),
           call=collect_call(expr, inTail, NULL, yield),
           name=if(!missing_(expr)) {
             yield("var", as.character(expr))
           })
  }
  collect_store <- function(dest, how, yield) {
    switch(mode(dest),
           call={
             # In a complex assignment like foo[bar] <- baz;
             # or equivalently `[`(foo, bar) <- baz;
             # R effectively expands it to:
             # foo <- `[<-`(foo, baz, bar)
             # which means "foo" should count as both "local"
             # and "var", and we should count a call to "[<-".
             collect_store(dest[[2]], how, yield)
             collect_call(
               as.call(c(list(as.name(paste0(dest[[1]], "<-"))),
                         as.list(dest)[-1])),
               inTail=FALSE, orig=NULL, yield=yield)
           },
           character=,
           name=
             yield(how, as.character(dest))
           )
  }
  for (i in locals) yield_("arg", i)
  collect_arg(body(fn), inTail=TRUE, yield=yield_)
}
