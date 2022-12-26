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
all_names <- function(fn,
                      types=c("call", "var", "arg", "local",
                              "store", "tail", "tramp", "hand"),
                      call="call" %in% types,
                      var="var" %in% types,
                      arg="arg" %in% types,
                      local="local" %in% types,
                      store="store" %in% types,
                      tail="tail" %in% types,
                      tramp="tramp" %in% types,
                      tailcall="tailcall" %in% types,
                      trampoline="trampoline" %in% types,
                      hand="hand" %in% types,
                      handler="handler" %in% types,
                      utility="utility" %in% types) {
  if (!is.function(fn)) stop("not a function")
  env <- environment(fn)
  args <- names(formals(fn)) %||% character(0)
  # less recursion if we only look for tailcalls
  nonTail <- any(call, var, local, store, utility)
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
    # i.e. trampolines and handlers don't need to register as tailcalls
    c(collect_head(expr[[1]], inTail=inTail),
      if(nonTail) concat(lapply(unname(expr[-1]), collect_arg,
                                inTail=FALSE)))
  }
  collect_lambda <- function(expr, inTail, orig=NULL) {
    # I use lambdas in a couple places still.
    # This is very a hack,
    # and would fail at arguments shadowing names etc.
    # I would have to thread more information through these
    # functions in order to track scope properly.
    # But really, the answer is turn those lambdas into regular nodes?
    tmp <- collect_arg(expr[[3]], inTail=TRUE) #!
    tmp <- tmp[!(tmp %in% names(expr[[2]]))]
    #tmp <- tmp[!(names(tmp) %in% c("local", "arg"))]
    tmp
  }
  collect_ordinary_call <- function(expr, inTail, orig=NULL) {
    c(if (tailcall && inTail) list(tailcall=c(list(expr), orig)),
      collect_weird_call(expr, inTail, orig))
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
                             collect_store(expr[[2]], "store")),
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
                   "function"={
                     collect_lambda(expr, inTail=inTail)
                   },
                   #some other primitive?
                   collect_ordinary_call(expr, inTail, orig)
                 )
               } else {
                 if (all(c("cont", "...") %in% names(formals(peek)))) {
                   # A trampoline-indirect call! Register both the target and
                   # the indirect.
                   handl <- match.call(peek, expr, expand.dots=FALSE,
                                       envir=as.environment(list(`...`=NULL)))
                   trampolined <- as.call(c(list(handl$cont), handl$...))
                   windup <- FALSE
                   if ("winding" %in% names(handl)) {
                     # windup takes TWO function pointers
                     woundup <- as.call(list(handl$winding))
                     windup <- TRUE
                     handl$winding <- NULL
                   }
                   handl$cont <- NULL
                   handl$... <- NULL
                   c(
                     if (tramp) c(tramp=as.character(trampolined[[1]])),
                     if (tramp && windup) c(tramp=as.character(woundup[[1]])),
                     if (hand) c(hand=as.character(expr[[1]])),
                     if (handler) list(handler=c(list(handl, expr), orig)),
                     collect_weird_call(handl, inTail,
                                        c(list(expr), orig)),
                     if (windup) collect_weird_call(woundup, inTail,
                                                     c(list(expr), orig)),
                     if (trampoline &&
                           # passing along "cont" from yield_ to pause_
                           # is not a new trampoline
                           !(as.character(trampolined[[1]]) %in% args))
                       c(list(trampoline=c(list(trampolined, expr), orig)),
                         if (windup) list(windup=c(list(woundup, expr), orig))),
                     collect_weird_call(trampolined, inTail,
                                        c(list(expr), orig)))
                 } else {
                   # if it's bound in the same context, note it as a utility call.
                   # an example is "trace"...
                   # which is going to get many duplicate entries because it's
                   # given as an argument to every constructor, hmm.

                   c(
                     if(utility && !inTail && identical(e, env)) {
                       c(utility = as.character(head))
                     },
                     collect_ordinary_call(expr, inTail, orig))
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
           name=if(var && !missing_(expr)) {
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
             else if (how == "store" && store)
               c(store=as.character(dest))
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

contains <- function(env, candidate, cmp=identical) {
  # FIXME: this should be a hashset or something?
  # maybe make something like memo::pointer_key(identity, list)("two")
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

paste.dropping.empty <- function(..., sep=".", collapse=NULL, recycle0=FALSE) {
  args <- list(...)
  result <- if (length(args) > 1) {
    a <- args[[1]]
    amask = (a == "") | (is.na(a))
    b = do.call("paste.dropping.empty",
                c(args[-1], list(sep=sep, recycle0=recycle0)))
    bmask = (b == "") | (is.na(b))
    ifelse(amask, b,
           ifelse(bmask, a,
                  paste(a, b, sep=sep, recycle0=recycle0)))
  } else if (length(args) == 1) {
    ifelse(is.na(args[[1]]), "", args[[1]])
  } else {
    character(0)
  }
  if (!is.null(collapse)) {
    paste(result[result != ""], collapse=collapse)
  } else result
}

condense.name <- function(name) {
  #input "name" is a character vector of branch labels.
  #for instance, if the path to get to a node takes branches with labels
  #start, cont, cont, cont, ifFalse, cont, cont, break
  #the condensed label will be "start.3.ifFalse.2.break"
  result <- with(rle(name), # with "values" and "lengths"
                 paste.dropping.empty(
                   ifelse(values=="cont", "", values),
                   ifelse(values != "cont" & lengths == 1, "", lengths),
                   sep="", collapse="."))
}

nullish <- function(x) length(x)==0
`%||%` <- function(x, y) if (nullish(x)) y else x
`%&&%` <- function(x, y) if (nullish(x)) x else y


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
# * `nodeProperties`: hash by node ID of list containing (at least)
#   * `reads`: nonlocal variables read from
#   * `writes`: nonlocal variables written to with <<-
#   * `calls`: closed-over functions called
#   * `tail`: functions called in tail position
#   * `args`: names in teh node's arg list
#   * `local`: variables updated with <-
# * `nodeEdgeProperties`: double hash table on node name and _local_ edge name
#     data is named list:
#     * `to`: Full name of destination node
#     * `type`: "tailcall" or "trampoline" or "handler"
#     * `call`: list of 1 element, tailcall verbatim
#       OR list of 2+ elements:
#          tailcall as interpreted
#          original trampoline call
# * `reverseEdges`: double hash table "to" then "from" node name
#                 data is local name
#
# * `contexts`: hash table (by generated name) of environment objects
# * `contextProperties`: hash by name of list:
#   * vars: vector of closed-over vars used by contained nodes.
#   * state: (TODO) closed-over vars that are the target of "<<-"
# * `contextNodes`: double hash by context name and contained node name
#                 data is TRUE
# * `nodeContexts`: hash by node ID, data is context ID
# * `orig`: the original call to async/gen
#' @export
walk <- function(gen) {
  nodes <- getStartSet(gen)
  nodeOrder <- names(nodes)
  nodes <- list2env(nodes, parent=emptyenv())
  orig <- getOrig(gen)
  iter <- icount()
  nodeProperties <- hashbag()
  reverseEdges <- hashbag()
  nodeEdgeProperties <- hashbag()
  nodeProperties <- hashbag()
  #do a DFS to collect the graph:
  # what storage used by each node / each context?
  varTypes <- c("call", "store", "local", "arg", "var", "utility",
                "tail", "tramp", "hand")
  doWalk <- function(thisNode, path, recurse=TRUE) {
    if (is.null(thisNodeName <- contains(nodes, thisNode))) {
      thisNodeName <- paste0("_", condense.name(path))
      assert(!exists(thisNodeName, envir=nodeProperties))
      nodes[[thisNodeName]] <<- thisNode
      nodeOrder <<- c(nodeOrder, thisNodeName)
    }
    if (exists(thisNodeName, envir=nodeProperties)) #already visited
      return(thisNodeName)
    trace_(sprintf("  Node: %s\n", thisNodeName))
    nodeProperties[[thisNodeName, "name"]] <<- thisNodeName
    vars <- by_name(
      all_names(nodes[[thisNodeName]], types=varTypes))
    for (type in varTypes) {
      nodeProperties[[thisNodeName, type]] <<- sort(vars[[type]])
    }
    # "reads" being anything used that's not local to the node
    reads <- c(setdiff(vars$var, union(vars$local, vars$arg)))
    nodeProperties[[thisNodeName, "read"]] <<- sort(reads)
    named <- function(x, name) structure(x %||% character(0),
                                         names=rep(name, length(x)))
    tails <- c(named(vars$hand, "hand"),
               named(vars$tramp, "tramp"),
               named(vars$tail, "tail"))
    locals <- c(named(vars$local, "local"),
                named(vars$arg, "arg"))
    # Don't try to follow a tailcall into an argument/local var;
    tails <- tails[!tails %in% locals]
    tails <- tails[!duplicated(tails)]

    for (i in seq_along(tails)) {
      tailcall <- tails[[i]]
      branchName <- as.character(tailcall[[1]][[1]])
      if (branchName %in% nodeProperties[[thisNodeName, "local"]]) next
      nextNode <- get(branchName, envir=environment(thisNode))
      nextNodeName <- doWalk(nextNode, c(path, branchName))
      trace_(sprintf("  Edge: %s :: %s -> %s\n", thisNodeName, branchName, nextNodeName))
      #if (thisNodeName == "_do_expr.1.awaited.then.1.ifTrue.1.do_finally.1") browser()
      reverseEdges[[nextNodeName]][[thisNodeName]] <<- branchName
      nodeEdgeProperties[[thisNodeName, branchName]] <<-
        list(to=nextNodeName, call=tailcall, type=names(tails)[[i]])
    }
    thisNodeName
  }
  trace_("Walking graph:\n")
  doWalk(nodes[[nodeOrder[[1]]]], "") # use path from start to name nodes.
  for (i in nodeOrder[-1]) { # and walk the rest to be sure
    doWalk(nodes[[i]], i)
  }
  trace_("Finding contexts:")
  # now we have collected a set of nodes and tables of
  # where each node can branch. Collect further information.
  # Contexts: what environment is each node in?
  contexts <- new.env(parent=emptyenv())
  nodeContexts <- new.env(parent=emptyenv())
  contextProperties <- hashbag()
  contextNodes <- hashbag()
  for (thisNodeName in nodeOrder) {
    thisNode <- nodes[[thisNodeName]]
    context <- environment(nodes[[thisNodeName]])
    if (is.null(contextName <- contains(contexts, context))) {
      contextName <- paste0(thisNodeName, "#")
      assert(!exists(contextName, envir=contexts))
      trace_(paste0("  Context: ", contextName, "\n"))
      contexts[[contextName]] <- context
    }
    contextNodes[[contextName,thisNodeName]] <- thisNodeName
    nodeContexts[[thisNodeName]] <- contextName
  }
  trace_("Analyzing contexts:\n")
  for (contextName in names(contexts)) {
    trace_(paste0("  Context: ", contextName, "\n"))
    # gather all nonlocal names used across this context
    for (kind in c("read", "store", "utility", "call", "tail", "tramp", "hand")) {
      contextProperties[[contextName, kind]] <-
        gatherVars(nodeProperties, contextNodes, contextName, kind)
    }
    context <- contexts[[contextName]]
    stores <- contextProperties[[contextName, "store"]]
    for (thisNodeName in names(contextNodes[[contextName]])) {
      # does the node have a local name in its context?
      nodeProperties[[thisNodeName, "localName"]] <- character(0)
      thisNode <- nodes[[thisNodeName]]
      for (nm in names(context)) {
        if (nm %in% stores) next # a state pointer isn't a stable name
        #watch out for unforced args like ifnotfound=stop("Not found")
        if (nm != "..." && is_forced_(nm, context)) {
          if (identical(context[[nm]], thisNode)) {
            trace_(sprintf("    Exit: %s -> %s\n", nm, thisNodeName))
            nodeProperties[[thisNodeName, "localName"]] <- nm
            if(nm == "R_") {
              nodeProperties[[thisNodeName, "Rexpr"]] <- expr(get("x", context))
            }
            break
          }
        }
      }
    }
  }

  list(nodes = nodes,
       nodeProperties = nodeProperties,
       nodeEdgeProperties = nodeEdgeProperties,
       reverseEdges = reverseEdges,
       contexts = contexts,
       contextProperties = contextProperties,
       nodeContexts = nodeContexts,
       contextNodes = contextNodes,
       orig = orig)
}

gatherVars <- function(nodeProperties, contextNodes, contextName, key) {
  unique(c(character(0), lapply(as.list(contextNodes[[contextName]]),
                  function(x) {nodeProperties[[x]][[key]]}), recursive=TRUE))
}
