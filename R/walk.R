concat <- function(l) do.call("c", l)
cc <- function(...) c(..., recursive=TRUE)

by_name <- function(vec) {
  names <- sort(unique(names(vec))) %||% character(0)
  lapply(structure(names, names=names),
         function(name) structure(unique(vec[names(vec) == name]),
                                  names=NULL))
}

unname <- function(x) `names<-`(x, NULL)

contains <- function(env, candidate, cmp=identical) {
  # this is only called if there's a bug in the package.
  for (key in sort(names(env))) {
    if (key == "...") next
    if (is_forced_(key, env))
      if (cmp(candidate, env[[key]]))
        return(key)
  }
  return(NA_character_)
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

## samefun <- function() {
##   identical(x, y) || (identical(formals(x), formals(y)))
##     && identical(environment(x), environment(y))
##     && identical(body(x), body(y))
## }

walk <- function(gen, forGraph=FALSE) {
  nodes <- getStartSet(gen)
  # override the apparent global names e.g. for "entry" and "return"
  # this is only necessary for graph drawing tho
  nodeStartNames <- vapply(nodes, find_global_name, "", emptyenv())
  nameOverrides <- list2env(structure(as.list(names(nodeStartNames)), names=nodeStartNames))
  nodeOrder <- names(nodes)
  nodes <- list2env(nodes, parent=emptyenv())
  orig <- summary(gen)$code
  nodeProperties <- new.env(parent=emptyenv())
  reverseEdges <- new.env(parent=emptyenv())
  nodeEdgeProperties <- new.env(parent=emptyenv())
  #do a DFS to collect the graph:
  # what storage used by each node / each context?
  varTypes <- c("call", "store", "local", "arg", "var", "read",
                "util", "tail", "tramp", "hand")
  doWalk <- function(thisNode, path, recurse=TRUE) {
    contextName <- get0(".contextName", environment(thisNode))
    localName <- find_local_name(thisNode)
    thisNodeName <- find_global_name(thisNode, nameOverrides)
    if (exists(thisNodeName, envir=nodeProperties)) {
      assert(identical(thisNode, nodes[[thisNodeName]]))
      return(thisNodeName)
    }
    trace_(sprintf("  Node: %s\n", thisNodeName))
    nodes[[thisNodeName]] <- thisNode
    nodeProperties[[thisNodeName]] <- new.env(parent=emptyenv())
    nodeEdgeProperties[[thisNodeName]] <- new.env(parent=emptyenv())
    nodeProperties[[thisNodeName]][["localName"]] <<- localName
    nodeOrder <<- c(nodeOrder, thisNodeName)
    nms <- all_names(nodes[[thisNodeName]])
    vars <- by_name(nms)
    for (type in varTypes) {
      nodeProperties[[thisNodeName]][[type]] <<- sort(vars[[type]])
    }
    named <- function(x, name) structure(x %||% character(0),
                                         names=rep(name, length(x)))
    tails <- nms[names(nms) %in% c("hand", "tramp", "tail", "wind")]
    tails <- tails[!duplicated(tails)]
    locals <- nms[names(nms) %in% c("local", "arg")]
    locals <- locals[!duplicated(tails)]
    assert(all(!tails %in% locals))

    if (forGraph) { # run another all_names to extract full calls
      tailcalls <-
        all_names(nodes[[thisNodeName]], forGraph=TRUE)
      tails2 <- vapply(tailcalls, function(x) as.character(x[[1]][[1]]), "")
      names(tails2) <- c(tailcall="tail", trampoline="tramp",
                         windup="wind", handler="hand")[names(tails2)]
      keep <- !(duplicated(tails2))
      tailcalls <- tailcalls[keep]
      tails2 <- tails2[keep]
      assert(setequal(tails, tails2)
             && all(names(sort(tails)) == names(sort(tails2))))
      tails <- tails2
    } else {
      tailcalls <- tails
    }
    for (i in seq_along(tails)) {
      tailcall <- tailcalls[[i]]
      branchName <- as.character(tailcall[[1]][[1]])
      nextNode <- get(branchName, envir=environment(thisNode))
      if (!is.function(nextNode)) stop("walk: tailcall to non-function")
      if (   is.null(environment(nextNode))
          || isNamespace(environment(nextNode))) {
        stop("walk: tailcall into package function")
      }
      nextNodeName <- doWalk(nextNode, c(path, branchName))
      trace_(sprintf("  Edge: %s :: %s -> %s\n", thisNodeName, branchName, nextNodeName))
      if (!exists(nextNodeName, envir=reverseEdges))
        reverseEdges[[nextNodeName]] <<- new.env(parent=emptyenv())
      reverseEdges[[nextNodeName]][[thisNodeName]] <<- branchName
      nodeEdgeProperties[[thisNodeName]][[branchName]] <<-
        list(to=nextNodeName, call=tailcall, type=names(tails)[[i]])
    }
    thisNodeName
  }
  trace_("Walking graph:\n")
  doWalk(nodes[[nodeOrder[[1]]]], "") # use path from start to name nodes.
  for (i in nodeOrder[-1]) { # and walk the rest to be sure
    doWalk(nodes[[i]], i)
  }
  trace_("Finding contexts:\n")
  # now we have collected a set of nodes and tables of
  # where each node can branch. Collect further information.
  # Contexts: what environment is each node in?
  contexts <- new.env(parent=emptyenv())
  nodeContexts <- new.env(parent=emptyenv())
  contextProperties <- new.env(parent=emptyenv())
  contextNodes <- new.env(parent=emptyenv())
  for (thisNodeName in nodeOrder) {
    thisNode <- nodes[[thisNodeName]]
    context <- environment(nodes[[thisNodeName]])
    contextName <- get(".contextName", context)
    if (exists(contextName, envir=contexts)) {
      if (!identical(contexts[[contextName]], context))
        stop(paste0("Context name collision: ", contextName))
    } else {
      trace_(paste0("  Context: ", contextName, "\n"))
      contexts[[contextName]] <- context
      contextProperties[[contextName]] <- new.env(parent=emptyenv())
    }
    nodeContexts[[thisNodeName]] <- contextName
    contextNodes[[contextName]][[thisNodeName]] <- thisNodeName
  }
  trace_("Analyzing contexts:\n")
  for (contextName in sort(names(contexts))) {
    trace_(paste0("  Context: ", contextName, "\n"))
    # gather all nonlocal names used across this context
    for (kind in c("read", "store", "util", "call", "tail", "tramp", "hand")) {
      contextProperties[[contextName]][[kind]] <-
        gatherVars(nodeProperties, contextNodes, contextName, kind)
    }
    context <- contexts[[contextName]]
    stores <- contextProperties[[contextName]][["store"]]
    for (thisNodeName in sort(names(contextNodes[[contextName]]))) {
      # does the node have a local name in its context?
      nodeProperties[[thisNodeName]][["localName"]] <- character(0)
      thisNode <- nodes[[thisNodeName]]
      for (nm in sort(names(context))) {
        if (nm %in% stores) next # a state pointer isn't a stable name
        #watch out for unforced args like ifnotfound=stop("Not found")
        if (nm != "..." && is_forced_(nm, context)) {
          if (identical(context[[nm]], thisNode)) {
            trace_(sprintf("    Exit: %s -> %s\n", nm, thisNodeName))
            nodeProperties[[thisNodeName]][["localName"]] <- nm
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
       orig = orig,
       nameOverrides = nameOverrides)
}

find_local_name <- function(fun) {
  if (is.null(name <- attr(fun, "localName"))) {
    name <- contains(environment(fun), fun)
    warning(paste0(get0(".contextName", environment(fun), ifnotfound="???"),
                   "__", name, " did not have a localName attribute")) # nocov
  } else {
    if (!identical(fun, get0(name, envir=environment(fun), ifnotfound="???"))) {
      stop(paste0(paste0(
        get0(".contextName", environment(fun), ifnotfound="???"),
        "__", name, "is misnamed?")))
    }
  }
  name
}

find_global_name <- function(fun, nameOverrides=emptyenv()) {
  if (is.null(name <- attr(fun, "globalName"))) {
    name <- paste0(get0(".contextName", environment(fun)),
                   "__", find_local_name(fun))
  }
  get0(name, nameOverrides, ifnotfound=name)
}

has_global_name <- function(fun) {
  if (is.null(gl <- attr(fun, "globalName"))) {
    if (is.null(ln <- attr(fun, "localName")))
      NA_character_
    else if (is.null(cn <- get0(".contextName",  envir=environment(fun),
                           ifnotfound=NULL)))
      NA_character_
    else
      paste0(cn, "__", ln)
  } else gl
}

gatherVars <- function(nodeProperties, contextNodes, contextName, key) {
  unique(c(character(0), lapply(as.list(contextNodes[[contextName]], all.names=TRUE),
                  function(x) {nodeProperties[[x]][[key]]}), recursive=TRUE))
}
