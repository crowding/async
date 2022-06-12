
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
getEntry.generator <- function(x) environment(environment(x$nextElem)$pump)$entry
#' @exportS3Method
getReturn.generator <- function(x) environment(x$nextElem)$return_
#' @exportS3Method
getStop.generator <- function(x) environment(x$nextElem)$stop_
#' @exportS3Method
getCurrent.generator <- function(x) environment(environment(x$nextElem)$pump)$cont
#' @exportS3Method
getOrig.generator <- function(x) expr(environment(x$nextElem)$orig)

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
# - tailcall: the entire calls in tail position
#             (a list, possibly with translated form for trampolines)
all_names <- function(fn,
                      types=c("call", "var", "arg", "local", "external", "tail"),
                      call="call" %in% types,
                      var="var" %in% types,
                      arg="arg" %in% types,
                      local="local" %in% types,
                      external="external" %in% types,
                      tail="tail" %in% types,
                      tailcall="tailcall" %in% types) {
  if (!is.function(fn)) stop("not a function")
  env <- environment(fn)
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
  collect_call <- function(expr, inTail, orig=NULL) {
    if (!inTail && !nonTail) return(character(0))
    head <- expr[[1]]
    switch(mode(head),
           character=,
           name={
             e <- locate_(head, env, mode="function", ifnotfound=NULL)
             if (!is.null(e)
                 && is_forced_(as.character(head), e)
                 && is.primitive(peek <- get(head, envir=e))) {
               switch(
                 as.character(head),
                 "=" =, "<-" = c(collect_store(expr[[2]], "local"),
                                 collect_arg(expr[[3]], inTail=FALSE)),
                 "<<-" = c(collect_store(expr[[2]], "external"),
                           collect_arg(expr[[3]], inTail=FALSE)),
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
                 c(if (tailcall && inTail) list(tailcall=c(list(expr), orig))
                   else character(0),
                   collect_head(expr[[1]], inTail=inTail),
                   if(nonTail) concat(lapply(unname(expr[-1]), collect_arg,
                                             inTail=FALSE)))
                 )
               } else { # not primitive, not forced, or not found?
                 switch(
                   as.character(head),
                   # tailcalls with >1 argument are trampolines.
                   "pause" = collect_call(expr[-1], inTail=inTail,
                                          orig=list(expr)),
                   "ret" = collect_call(expr[-1], inTail=inTail,
                                        orig=list(expr)),
                   "unwind" = collect_call(expr[-1], inTail=inTail,
                                           orig=list(expr)),
                   # FIXME: how to get the link to STOP??
                   "windup" = collect_call(expr[c(-1,-2)], inTail=inTail,
                                           orig=list(expr)),
                   # general function calls
                   c(if (tailcall && inTail) list(tailcall=c(list(expr), orig))
                     else character(0),
                     collect_head(expr[[1]], inTail=inTail),
                     if(nonTail)
                       concat(lapply(unname(expr[-1]),
                                     collect_arg, inTail=FALSE)))
                 )
               }
             },
           # something other than a name in a call head?
           if(nonTail) c(collect_head(expr[[1]], FALSE),
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
  c(if (arg) structure(names=rep("arg", length(formals(fn))),
                       names(formals(fn)) %||% character(0)),
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
    tails <- all_names(thisNode, "tailcall")
    for (tailcall in tails) {
      branchName <- as.character(tailcall[[1]][[1]])
      nextNode <- get(branchName, envir=environment(thisNode))
      nextNodeName <- doWalk(nextNode, c(path, branchName))
      #cat(sprintf("%s:%s -> %s\n", thisName, branchName, nextNodeName))
      reverseEdges[[nextNodeName]][[thisName]] <<- TRUE
      edgeProperties[[thisName, nextNodeName]] <<-
        list(label=branchName, call=tailcall)
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


## makeGraph
# Code for outputting a DOT file given data collected above
quoted <- function(name) paste0('"', gsub("([\\\\\\\"])", "\\\\\\1", name, ""), '"')
block <- function(block) function(name, ...) {
  c(paste0(block, " ", quoted(name), " { "), paste0("  ", c(...)), "}")}
group <- function(items) paste0("{", paste0(quoted(items), collapse=" "), "}")
attrs <- function(...) {
  x = c(...)
  if(length(x) > 0)
    paste0("[", paste0(names(x), "=", quoted(x), collapse=", "), "]")
  else character(0)}
nodeAttrs <- function(nodeGraph, nodeName) {
  label <- nodeGraph$nodeProperties[[nodeName]]$localName %||% ""
  c(switch(
      label,
      "R_"=c(label=paste0(deparse(expr(environment(nodeGraph$nodes[[nodeName]])$x)),
                          collapse="\n"),
             fontname="DejaVu Sans Mono Bold", style="filled",
             fontcolor="lightgreen", color="gray20", labeljust="l"),
      ";"=c(shape="circle", fixedsize="true",
            width=0.25, height=0.25, label=";"),
      c(label=label, style="filled,rounded", color="gray70")),
    switch(
      nodeName,
      START=c(shape="doubleoctagon", color="darkgreen", style="filled",
              fontcolor="lightgreen", margin="0,0", fixedsize="false"),
      STOP=c(shape="doubleoctagon", color="darkred", style="filled",
             fontcolor="pink",  margin="0,0", fixedsize="false"),
      RETURN=c(shape="doubleoctagon", color="darkblue", style="filled",
               fontcolor="lightblue", margin="0,0", fixedsize="false")))}
node <- function(nodeGraph, nodeName) {
  paste(quoted(nodeName),
        attrs(nodeAttrs(nodeGraph, nodeName)))}
nodes <- function(nodeGraph, nodes) {
  concat(lapply(nodes, function(n) node(nodeGraph,n)))}
read <- function(from, field, to)
  paste0(quoted(from), ":", field, " -> ", quoted(to))
store <- function(from, to, field)
  paste0(quoted(from), " -> ", quoted(to), ":", field)
storage <- function(nodeGraph, context) {
  # make a record node for context storage
  # they say record-based nodes are obsoleted by html-style labels
  # but I'll start with this...
  varNames <- nodeGraph$contextProperties[[context]]$vars
  if (length(varNames) == 0) NULL else {
    c(paste(
      quoted(paste0(context, "_", "var")),
      attrs(shape="record",
            label=paste0(
              paste0("<", varNames, ">", varNames, collapse="|")))),
      # "{", paste0("<", varNames, ">", varNames, collapse="|"), "}"))),
      concat(lapply(
        names(nodeGraph$contextNodes[[context]]),
        function(node) {
          reads <- nodeGraph$nodeProperties[[node]]$reads
          stores <- nodeGraph$nodeProperties[[node]]$stores
          ## edges for the storage
          c(
            if (length(reads) > 0)
              paste(read(paste0(context, "_var"), reads, node),
                    attrs(penwidth=1, color="slateblue3", arrowsize=0.5, arrowhead="dot")),
            if (length(stores) > 0)
              paste(store(node, paste0(context, "_var"), stores),
                    attrs(penwidth=1, color="orange3", arrowsize=0.5, arrowhead="dot"))
          )})))}}
subgraph <- block("subgraph")
subgraphs <- function(nodeGraph) {
  concat(
    lapply(
      sort(names(nodeGraph$contexts)),
      function(sgName) {
        nodeNames <- names(nodeGraph$contextNodes[[sgName]])
        # draw a box IF there are multiple nodes OR there is storage.
        # but NOT if it's a "passthrough R code" node (which has storage at
        # compile time?)
        label <- nodeGraph$nodeProperties[[nodeNames[[1]]]]$localName %||% ""
        if ((
          ( (length(nodeNames) > 1)
            || (length(nodeGraph$contextProperties[[sgName]]$vars) > 0) )
          && ( label != "R_" )
        )) {
          subgraph(paste0("cluster_", sgName),
                   props(label="", shape="box", style="dashed,rounded",
                         #rank="same",
                         margin=12, penwidth=3, color="gray70"),
                   nodes(nodeGraph,
                         sort(names(nodeGraph$contextNodes[[sgName]]))),
                   storage(nodeGraph, sgName))
        } else {
          # no box, draw a single node
          node(nodeGraph, nodeNames[[1]])
        }
      }
    )
  )
}
edgeAttrs <- function(nodeGraph, from, to) {
  # edge properties.
  # bolder if carrying a value
  # outgoing label.
  props <- nodeGraph$edgeProperties[[from, to]]
  c(if (props$label=="cont" || length(nodeGraph$edgeProperties[[from]]) <= 1)
    c(taillabel="") else c(taillabel=props$label),
    if(length(props$call[[1]]) > 1) #tailcall carries value
      c(penwidth="2", arrowhead="normal")
    else c(color="gray50", penwidth="2"),
    ## if (identical(nodeGraph$nodeProperties[[to]]$localName, ";"))
    ##   c(arrowhead="none"),
    if (length(props$call) > 1) { #"special" tailcalls
      c(arrowhead="odot", taillabel=" ", labelangle=0, fontsize=15, arrowsize=2.25,
        labeldistance=.9, fontcolor="blue",
        switch(as.character(props$call[[2]][[1]]),
               ret=c(headlabel="⮍"),
               pause=c(headlabel="⏸",labeldistance=0.8),
               windup=c(headlabel="⤽", fontsize=20),
               unwind=c(headlabel="⤼", fontsize=20)))
    }
  )}
edge <- function(nodeGraph, from, to) {
  concat(lapply(to, function(edgeTo) {
    paste(quoted(from), "->", quoted(edgeTo),
          attrs(edgeAttrs(nodeGraph, from, edgeTo)))
  }))}
edges <- function(nodeGraph) {
  concat(lapply(all_indices(nodeGraph$edgeProperties),
                function(i) edge(nodeGraph, i[1], i[2])))
}
graph <- block("graph")
digraph <- block("digraph")

props <- function(...) {
  x = c(...)
  paste(names(x), "=", quoted(x))}
defaults <- function(type, ...) {
  paste(type, attrs(c(...)))}
make_dot <- function(nodeGraph) {
  digraph(
    "G",
    props(bgcolor="lightgray", margin=0, pad=0.25,
          nodesep=0.3, ranksep=0.4, newrank="true", # packmode="graph",
          labeljust="l", fontname="DejaVu Sans Mono Book", rankdir="TB"),
    defaults("edge", fontname="DejaVu Sans Bold", arrowhead="normal",
             arrowsize=0.6, fontsize=8),
    defaults("node", fontname="DejaVu Sans Bold",
             margin="0.1,0.1", shape="box", bgcolor="white",
             height=0.2, width=0.2, color="gray60", penwidth=2),
    NULL %&&% subgraph("cluster_label",
                       paste("label",
                             attrs(shape="box", labeljust="l",
                                   fontname="DejaVu Sans Mono Book",
                                   labeljust="l", penwidth=0,
                                   label=paste0(deparse(nodeGraph$orig),
                                                collapse="\n")))),
    subgraphs(nodeGraph),
    edges(nodeGraph)
  )
}


## digraph structs {
##   node [shape=record];
##   struct1 [label="<f0> left|<f1> mid&#92; dle|<f2> right"];
##                                struct2 [label="<f0> one|<f1> two"];
##   struct3 [label="hello&#92;nworld |{ b |{c|<here> d|e}| f}| g | h"];
##              struct1:f1 -> struct2:f0;
##   struct1:f2 -> struct3:here;
## }

## writeHtml <- function(nodes) {
##   arg_ <- arg(nodes)
##   tags <- all.names(expr(arg_),
##                     functions=TRUE, unique=TRUE)
##   x <- new.env(expr(arg_), )
## }


#' Draw a graph representation of a generator.
#'
#' @param x a generator or async object.
#' @param filename The file to write to.
#'
#' makeGraph will write a Graphviz DOT format file describing the
#' given generator or async/await block. If you have Graphviz
#' installed you can then use the `dot` command to render the graph.
#'
#' @examples
#' randomWalk <- gen({x <- 0; repeat {yield(x); x <- x + rnorm(1)}})
#' \dontrun{
#' #write a dotfile
#' makeGraph(randomWalk, "rwalk.dot")
#'
#' #compile graph to ODF:
#' system("dot -Tpdf rwalk.dot > rwalk.pdf")
#'
#' #or, display it in R with the Rgraphviz package:
#' g <- Rgraphviz::agread("rwalk.dot")
#' Rgraphviz::plot(g)
#' }
#' @param x A [generator][gen] or [async] object.
#' @param file Default will write to stdout. See [cat].
#' @param sep Line separator; defaults to `"\n"`. See [cat].
#' @param ... Other arguments to be passed to [cat].
#' @return a list containing information about the async/iterator.
#' @export
makeGraph <- function(x, file="", sep="\n", ...) {
  UseMethod("makeGraph")
}

#' @exportS3Method
makeGraph.generator <- function(x, file="", sep="\n", ...) {
  graph <- walk(x)
  cat(make_dot(graph), file=file, sep=sep, ...)
  invisible(graph)
}

#' @exportS3Method
makeGraph.async <- function(x, file="", sep="\n", ...) {
  graph <- walk(x)
  cat(make_dot(graph), file=file, sep=sep, ...)
  invisible(graph)
}
