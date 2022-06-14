# Code for walking over an async/generator and gathering information about its
# nodes and graphs.

#' @export
getEntry <- function(x) UseMethod("getEntry")
#' @export
getReturn <- function(x) UseMethod("getReturn")
#' @export
getStop <- function(x) UseMethod("getStop")
#' @export
getCurrent <- function(x) UseMethod("getStop")
#' @export
getOrig <- function(x) UseMethod("getOrig")

#' @exportS3Method
getEntry.generator <- function(x) environment(environment(x)$pump)$entry
#' @exportS3Method
getReturn.generator <- function(x) environment(x)$return_
#' @exportS3Method
getStop.generator <- function(x) environment(x)$stop_
#' @exportS3Method
getCurrent.generator <- function(x) environment(environment(x)$pump)$cont
#' @exportS3Method
getOrig.generator <- function(x) expr(environment(x)$orig)

#' @exportS3Method
getEntry.async <- function(x) environment(x$state$pump)$entry
#' @exportS3Method
getReturn.async <- function(x) x$state$resolve
#' @exportS3Method
getStop.async <- function(x) x$state$reject
#' @exportS3Method
getCurrent.async <- function(x) environment(x$state$pump)$cont
#' @exportS3Method
getOrig.async <- function(x) x$orig

concat <- function(l) do.call("c", l)
cc <- function(...) c(..., recursive=TRUE)

by_name <- function(vec) {
  names <- sort(unique(names(vec))) %||% character(0)
  lapply(structure(names, names=names),
         function(name) structure(unique(vec[names(vec) == name]),
                                  names=NULL))
}

# Scan a function and return all names used, labeled by their
# (possibly overlapping) "roles":
#
# - arg: names defined as arguments
# - var: names appearing in arguments of calls
# - call: names appearing at head of calls
# - local: names appearing as target of <-
# - external: names appearing as target of <<-
# - tail: names of calls in "tail position"
# - tramp: names that are passed to "cont" of a tailcall (i.e. a trampoline)
# - tailcall: the entire calls in tail position.
#             (a list, possibly with original form for trampolines)
# - trampoline: entire trampolined calls
#             (also a list with orginal forms)
all_names <- function(fn,
                      types=c("call", "var", "arg", "local",
                              "external", "tail", "tramp"),
                      call="call" %in% types,
                      var="var" %in% types,
                      arg="arg" %in% types,
                      local="local" %in% types,
                      external="external" %in% types,
                      tail="tail" %in% types,
                      tramp="tramp" %in% types,
                      tailcall="tailcall" %in% types,
                      trampoline="trampoline" %in% types,
                      handler="handler" %in% types) {
  if (!is.function(fn)) stop("not a function")
  env <- environment(fn)
  args <- names(formals(fn)) %||% character(0)
  # less recursion if we only look for tailcalls
  nonTail <- any(call, var, local, external)
  collect_head <- function(expr, inTail) {
    if (!inTail && !nonTail) return(character(0))
    switch(mode(expr),
           call= {
             switch(
               mode(expr[[1]]),
               name=, character= {
                 # could be a "::" or ":::"
                 if (as.character(expr[[1]]) %in% c("::", ":::")) {
                   name <- paste0(as.character(expr[[2]]),
                                  as.character(expr[[1]]),
                                  as.character(expr[[3]]))
                   c(if(call) c(call=name),
                     if(inTail && tail) c(tail=name))
                 } else {
                   collect_call(expr, inTail=FALSE)
                 }
               },
               collect_call(expr, inTail=FALSE)
             )
           },
           character=,
           name=c(
             if(call) c(call=as.character(expr)),
             if(inTail && tail) c(tail=as.character(expr))),
           character(0)
           )
  }
  collect_weird_call <- function(expr, inTail, orig=NULL) {
    c(collect_head(expr[[1]], inTail=inTail),
      if(nonTail) concat(lapply(unname(expr[-1]), collect_arg,
                                inTail=FALSE)))
  }
  collect_ordinary_call <- function(expr, inTail, orig=NULL) {
    c(if (tailcall && inTail) list(tailcall=c(list(expr), orig)),
      collect_head(expr[[1]], inTail=inTail),
      if(nonTail) concat(lapply(unname(expr[-1]), collect_arg,
                                inTail=FALSE)))
  }
  collect_call <- function(expr, inTail, orig=NULL) {
    if (!inTail && !nonTail) return(character(0))
    head <- expr[[1]]
    switch(mode(head),
           character=,
           name={
             isArg <- as.character(head) %in% args
             if (!isArg)
               e <- locate_(head, env, mode="function", ifnotfound=NULL)
             if (!isArg && !is.null(e)
                 && is_forced_(as.character(head), e)) {
               peek <- get(head, envir=e)
               if (is.primitive(peek)) {
                 switch(
                   as.character(head),
                   "=" =, "<-" = c(collect_arg(expr[[3]], inTail=FALSE),
                                   collect_store(expr[[2]], "local")),
                   "<<-" = c(collect_arg(expr[[3]], inTail=FALSE),
                             collect_store(expr[[2]], "external")),
                   "if" = c(collect_arg(expr[[2]], inTail=FALSE),
                            collect_arg(expr[[3]], inTail=inTail),
                            if (length(expr) >= 4)
                              collect_arg(expr[[4]], inTail=inTail)),
                   # the argument to return() is not considered to be
                   # in the tail because it wouldn't inline into a state
                   # machine switch statement that way.
                   "return" = collect_arg(expr[[2]], inTail=FALSE),
                   "while" = c(collect_arg(expr[[2]], inTail=FALSE),
                               collect_arg(expr[[3]], inTail=FALSE)),
                   "for" = c(collect_arg(expr[[2]], inTail=FALSE),
                             collect_arg(expr[[3]], inTail=FALSE),
                             collect_arg(expr[[4]], inTail=FALSE)),
                   "("= collect_arg(expr[[1]], inTail=inTail),
                   "{"= c(
                     if(nonTail)
                       concat(lapply(unname(expr[c(-1,-length(expr))]),
                                     collect_arg, inTail=FALSE)),
                     collect_arg(expr[[length(expr)]], inTail=inTail)),
                   "||"=,"&&"=c(collect_arg(expr[[2]], inTail=FALSE),
                                collect_arg(expr[[3]], inTail=inTail)),
                   "switch"=c(collect_arg(expr[[2]], inTail=FALSE),
                              if(inTail || nonTail) concat(
                                lapply(unname(expr[-1]), collect_arg,
                                       inTail=inTail))),
                   "function"=character(0),
                   #some other primitive?
                   collect_ordinary_call(expr, inTail, orig)
                 )
               } else {
                 # Exists but not primitive. If a tailcall, peek at the
                 # formals. If the formals have "..." then it is a
                 # trampoline call.
                 if (inTail && "..." %in% names(formals(peek))) {
                   # trampoline! Register both the target and the indirect.
                   handl <- match.call(peek, expr, expand.dots=FALSE)
                   trampolined <- as.call(c(list(handl$cont), handl$...))
                   handl$cont <- NULL
                   handl$... <- NULL
                   c(
                     if (tramp) c(tramp=as.character(trampolined[[1]])),
                     if (handler) list(handler=c(list(handl, expr), orig)),
                     if (trampoline)
                       list(trampoline=c(list(trampolined, expr), orig)),
                     collect_weird_call(handl, inTail, c(list(expr), orig)),
                     collect_weird_call(trampolined, inTail, c(list(expr), orig))
                     )
                 } else {
                   collect_ordinary_call(expr, inTail, orig)
                 }
               }
             } else {
               # name not found, or an arg
               collect_ordinary_call(expr, inTail, orig)
             }
           },
           # something other than a name in a call head?
           if(nonTail)
             c(collect_head(expr[[1]], FALSE),
               concat(lapply(unname(expr[-1]), collect_arg, inTail=FALSE))))
  }
  collect_arg <- function(expr, inTail) {
    if (!inTail && !nonTail) return(character(0))
    switch(mode(expr),
           call=collect_call(expr, inTail),
           name=if(var) {
             c(var=unname(as.character(expr)))
           } else character(0),
           character(0))
  }
  collect_store <- function(dest, how) {
    switch(mode(dest),
           call=c(
             # In a complex assignment like foo[bar] <- baz;
             # or equivalently `[`(foo, bar) <- baz;
             # R effectively expands it to:
             # foo <- `[<-`(foo, baz, bar)
             # which means "foo" should count as both "local"
             # and "var", and we should count a call to "[<-".
             collect_store(dest[[2]], how),
             collect_call(
               as.call(c(list(as.name(paste0(dest[[1]], "<-"))),
                         as.list(dest)[-1])),
               inTail=FALSE)),
           character=,
           name={
             if (how == "local" && local)
               c(local=as.character(dest))
             else if (how == "external" && external)
               c(external=as.character(dest))
           },
           character(0))}
  c(if (arg) structure(args, names=rep("arg", length(args))),
    collect_arg(body(fn), inTail=TRUE))
}

unname <- function(x) `names<-`(x, NULL)

by_class <- function(vec) {
  out <- list()
  for (class in unique(names(vec))) {
    out[[class]] <- unname(vec[names(vec) == class])
  }
  out
}

# memo::pointer_key(identity, list)("two")
contains <- function(env, candidate, cmp=identical) {
  # FOXME: this should be a hashset or something
  for (key in names(env))
    if (cmp(candidate, env[[key]]))
      return(key)
  return(NULL)
}

# and I need an incremental make.unique?
make.unique.wrt <- function(x, existing) {
  u <- make.unique(c(existing, x))
  if (length(existing) > 0)
    u <- u[-seq_len(length(existing))]
  u
}

condense.name <- function(name) {
  #input "name" is a character vector of branch names
  with(rle(name),
       paste0(ifelse(values=="cont", "", values),
              ifelse(values != "cont" & lengths == 1, "", lengths),
              collapse="."))
}

## Hashbag
# is a multi-level dictionary
hashbag <- function() {
  structure(new.env(parent=emptyenv()), class="hashbag")
}
nullish <- function(x) length(x)==0
`%||%` <- function(x, y) if (nullish(x)) y else x
`%&&%` <- function(x, y) if (nullish(x)) x else y

#' @export
`[[.hashbag` <- function(x, ..., ifnotfound=structure(list(), class="hashbag")) {
  indices <- list(...)
  if (!nullish(x) && exists(indices[[1]], envir=x)) {
    if (length(indices) > 1) {
      do.call(`[[`, c(list(get(indices[[1]], envir=x)), indices[-1]))
    } else {
      get(indices[[1]], envir=x)
    }
  } else {
    ifnotfound
  }
}
#' @export
`[[<-.hashbag` <- function(x, ..., value) {
  indices <- list(...)
  if (nullish(x)) x <- hashbag()
  if (length(indices) > 1) {
    if (exists(indices[[1]], envir=x)) {
      val <- x[[ indices[[1]] ]]
    } else {
      val <- hashbag()
    }
    val <- do.call("[[<-", c(quote(val), indices[-1], list(value=value)))
  } else {
    val <- value
  }
  assign(indices[[1]], val, envir=x)
  x
}

all_indices <- function(x) UseMethod("all_indices")

#' @export
all_indices.hashbag <- function(x) {
  l <- as.list.environment(x, all.names=TRUE, sorted=TRUE)
  n <- names(l)
  indices <- mapply(n, l, SIMPLIFY=FALSE, USE.NAMES=FALSE, FUN=function(k, v) {
    if (!nullish(v) && "hashbag" %in% class(v)) {
      mapply(`c`, k, all_indices(v), USE.NAMES=FALSE, SIMPLIFY=FALSE)
    } else list(k)
  })
  concat(indices)
}

##  Walk
# collect all nodes and edges and give them names,
# nodes being named according to the sequence of branches.
# Collect other interesting information about the nodes, including
# if they share a context (enclosing environment) and what context
# variables are used.
#
# Output is a named list of data structures:
#
# * `nodes`: hash table of ID (generated) to node (function objects)
# * `nodeProperties`: hash by node ID of list:
#   * `reads`: nonlocal variables read from
#   * `writes`: nonlocal variables written to (with <<-)
#
# * `edgeProperties`:
#   * double hash table, "from" node name and "to" node name
#     data is named list:
#     * `label`: (local branch name)
#     * `type`: "tailcall" or "trampoline" or "handler"
#     * `call`: list of 1 element, tailcall verbatim
#       OR list of 2+ elements:
#          tailcall as interpreted
#          original trampoline call
# * `reverseEdges`: double hash table "to" then "from" node name
#                 data is TRUE
#
# * `contexts`: hash table (by generated name) of environment objects
# * `contextProperties`: hash by name of list:
#   * vars: vector of closed-over vars used by contained nodes.
# * `contextNodes`: double hash by context name and contained node name
#                 data is TRUE
# * `nodeContexts`: hash by node ID, data is context ID
# * `orig`: the original call to async/gen
#' @export
walk <- function(gen) {
  nodes <- new.env(parent=emptyenv())
  nodes$START <- getEntry(gen)
  nodes$STOP <- getStop(gen)
  nodes$RETURN <- getReturn(gen)
  orig <- getOrig(gen)
  iter <- icount()
  nodeProperties <- new.env(parent=emptyenv())
  reverseEdges <- hashbag()
  edgeProperties <- hashbag()
  nodeProperties <- hashbag()
  #do a DFS to collect the graph:
  doWalk <- function(thisNode, path) {
    if (is.null(thisName <- contains(nodes, thisNode))) {
      thisName <- condense.name(path)
      assert(!exists(thisName, envir=nodeProperties))
      nodes[[thisName]] <<- thisNode
    }
    if (exists(thisName, envir=nodeProperties)) #already visited
      return(thisName)
    nodeProperties[[thisName]]$name <<- thisName
    tails <- all_names(thisNode, c("tailcall", "trampoline", "handler"))
    for (i in seq_along(tails)) {
      tailcall <- tails[[i]]
      branchName <- as.character(tailcall[[1]][[1]])
      nextNode <- get(branchName, envir=environment(thisNode))
      nextNodeName <- doWalk(nextNode, c(path, branchName))
      #cat(sprintf("%s:%s -> %s\n", thisName, branchName, nextNodeName))
      reverseEdges[[nextNodeName]][[thisName]] <<- TRUE
      edgeProperties[[thisName, nextNodeName]] <<-
        list(label=branchName, call=tailcall, type=names(tails)[[i]])
    }
    thisName
  }
  doWalk(nodes$START, ".")
  for (i in names(nodes)) {
    doWalk(nodes[[i]], i)
  }
  # now we have collected a set of nodes and tables of
  # where each node can branch. Collect further information.
  # Contexts: what environment is each node in?
  contexts <- new.env(parent=emptyenv())
  nodeContexts <- new.env(parent=emptyenv())
  contextProperties <- new.env(parent=emptyenv())
  contextNodes <- hashbag()
  for (thisNodeName in names(nodes)) {
    thisNode <- nodes[[thisNodeName]]
    context <- environment(nodes[[thisNodeName]])
    if (is.null(contextName <- contains(contexts, context))) {
      contextName <- paste0("ctx.", thisNodeName)
      assert(!exists(contextName, envir=contexts))
      #cat("context: ", contextName, "\n")
      contexts[[contextName]] <- context
    }
    contextNodes[[contextName]][[thisNodeName]] <- thisNodeName
    nodeContexts[[thisNodeName]] <- contextName

    #does the node have a name in its context?
    nodeProperties[[thisNodeName]]$localName <- character(0)
    for (nm in names(context)) {
      #watch out for unforced args like ifnotfound=stop("Not found")
      if (nm != "..." && is_forced_(nm, context)) {
        if (identical(context[[nm]], thisNode)) {
          #cat(thisNodeName, " is called ", nm, "\n")
          nodeProperties[[thisNodeName]]$localName <- nm
          if(nm == "R_") {
            nodeProperties[[thisNodeName]]$Rexpr <- expr(get("x", context))
          }
        }
      }
    }
  }
  # what storage used by each node / each context?
  for (cxt in names(contexts)) {
    cxtVars <- sort(unique(concat(lapply(
      names(contextNodes[[cxt]]), function(thisNodeName) {
        x <- all_names(nodes[[thisNodeName]],
                  types=c("external", "local", "arg", "var"))
        vars <- by_name(
          all_names(nodes[[thisNodeName]],
                    types=c("external", "local", "arg", "var")))
        reads <- setdiff(vars$var, union(vars$local, vars$arg))
        stores <- vars$external
        nodeProperties[[thisNodeName]]$reads <<- sort(reads)
        nodeProperties[[thisNodeName]]$stores <<- sort(stores)
        sort(union(reads, stores))
      }
    ))))
    contextProperties[[cxt]] <- list(vars=cxtVars)
  }
  list(nodes=nodes,
       nodeProperties=nodeProperties,
       edgeProperties=edgeProperties,
       reverseEdges=reverseEdges,
       contexts=contexts,
       contextProperties=contextProperties,
       nodeContexts=nodeContexts,
       contextNodes=contextNodes,
       orig=orig)
}

