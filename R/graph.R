
#' @export
getEntry <- function(x) UseMethod("getEntry")
getReturn <- function(x) UseMethod("getReturn")
getStop <- function(x) UseMethod("getStop")

## environment(genprimes$nextElem)
## ls(environment(genprimes$nextElem)) # make_generator closure
## environment(environment(genprimes$nextElem)$pump) # make_pump closure
## ls(environment(environment(genprimes$nextElem)$pump)) # here's all the pump state
## entry <- environment(environment(genprimes$nextElem)$pump)$cont # the entry point, it's an R()

#environment(environment(genprimes$nextElem)$pump)

#' @exportS3Method
getEntry.generator <- function(x) environment(environment(x$nextElem)$pump)$entry
#' @exportS3Method
getReturn.generator <- function(x) environment(x$nextElem)$return_
#' @exportS3Method
getStop.generator <- function(x) environment(x$nextElem)$stop_

concat <- function(l) do.call("c", l)

# Scan an expression and return all names used, labeled by their
# "roles": var, call, local, state, tailcall
all_names <- function(expr,
                      types=c("call", "var", "local", "state", "tail"),
                      call="call" %in% types,
                      var="var" %in% types,
                      local="var" %in% types,
                      state="state" %in% types,
                      tail="tail" %in% types,
                      tailcall="tailcall" %in% types) {
  # less recursion if we only look for tailcalls
  nonTail <- any(call, var, local, state)
  collect_head <- function(expr, inTail) {
    if (!inTail && !nonTail) return(character(0))
    switch(mode(expr),
           call= collect_call(expr, inTail=FALSE),
           character=,
           name=c(if(call) c(call=as.character(expr)),
                  if(inTail && tail) c(tail=as.character(expr))),
           character(0))
  }
  collect_call <- function(expr, inTail) {
    if (!inTail && !nonTail) return(character(0))
    switch(mode(expr[[1]]),
           character=,
           name=switch(
             as.character(expr[[1]]),
             # special control flows
             "=" =, "<-" = c(if (local) c(local=collect_store(expr[[2]])),
                             collect_arg(expr[[3]], inTail=FALSE)),
             "<<-" = c(if (state) c(state=collect_store(expr[[2]])),
                       collect_arg(expr[[3]], inTail=FALSE)),
             "if" = c(collect_arg(expr[[2]], inTail=FALSE),
                      collect_arg(expr[[3]], inTail=inTail),
                      if (length(expr) >= 4) collect_arg(expr[[4]], inTail=inTail)),
             "return" = collect_arg(expr[[2]], inTail=TRUE),
             "while" = c(collect_arg(expr[[2]], inTail=FALSE),
                         collect_arg(expr[[3]], inTail=inTail)),
             "for" = c(collect_arg(expr[[2]], inTail=FALSE),
                       collect_arg(expr[[3]], inTail=FALSE),
                       collect_arg(expr[[4]], inTail=inTail)),
             "("= collect_arg(expr[[1]], inTail=inTail),
             "{"= c(concat(lapply(expr[c(-1,-length(expr))],
                                  collect_arg, inTail=FALSE)),
                    collect_arg(expr[[length(expr)]], inTail=inTail)),
             "||"=,"&&"=c(collect_arg(expr[[2]], inTail=FALSE),
                          collect_arg(expr[[3]], inTail=inTail)),
             "switch"=c(collect_arg(expr[[2]], inTail=FALSE),
                        concat(lapply(expr[-1], collect_arg, inTail=inTail))),
             "function"=character(0),
             # special tailcalls
             "pause" = collect_call(expr[-1], inTail=inTail),
             "ret" = collect_call(expr[-1], inTail=inTail),
             # general function calls
             c(if (tailcall && inTail) list(tailcall=expr) else character(0),
               collect_head(expr[[1]], inTail=inTail),
               concat(lapply(expr[-1], collect_arg, inTail=FALSE)))
           ),
           # a call with a call in its head?
           c(collect_head(expr[[1]], FALSE),
             concat(lapply(expr[-1], collect_arg, inTail=inTail)))
           )
  }
  collect_arg <- function(expr, inTail) {
    if (!inTail && !nonTail) return(character(0))
    switch(mode(expr),
           call=collect_call(expr, inTail),
           name=if(var) c(var=as.character(expr)) else character(0),
           character(0))}
  collect_store <- function(dest) {
    if (!inTail && !nonTail) return(character(0))
    switch(mode(dest),
           call=collect_store(dest[[2]]),
           character=dest,
           name=as.character(dest),
           character(0))}
  collect_arg(expr, inTail=TRUE)
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
  # this should be a hashset or something
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

# objDict <-

# `[[`.objDict
# `[[`.objDict <- function(x, 

##  Walk
# collect all nodes and edges and give them names,
# nodes being named according to the sequence of branches.
# Collect other interesting information about the nodes.
#' @export
walk <- function(thing) UseMethod("walk")

#' @exportS3Method
walk <- function(gen) {
  start <- getEntry(gen)
  stop_ <- getStop(gen)
  stopName <- NULL
  return_ <- getReturn(gen)
  returnName <- NULL
  iter <- icount()
  nodes <- new.env(parent=emptyenv())
  nodes$START <- start; nodes$STOP <- stop_; nodes$RETURN <- return_
  nodeProperties <- new.env(parent=emptyenv())
  edges <- hashbag()
  reverseEdges <- hashbag()
  edgeProperties <- hashbag()
  nodeProperties <- hashbag()
  doWalk <- function(thisNode, path) {
    if (is.null(thisName <- contains(nodes, thisNode))) {
      thisName <- condense.name(path)
      assert(!exists(thisName, envir=nodeProperties))
      nodes[[thisName]] <<- thisNode
    }
    if (exists(thisName, envir=nodeProperties)) #already visited
      return(thisName)
    nodeProperties[[thisName]]$name <<- thisName
    tails <- all_names(body(thisNode), "tailcall")
    for (tailcall in tails) {
      branchName <- as.character(tailcall[[1]])
      nextNode <- get(branchName, envir=environment(thisNode))
      nextNodeName <- doWalk(nextNode, c(path, branchName))
      #cat(sprintf("%s:%s -> %s\n", thisName, branchName, nextNodeName))
      reverseEdges[[nextNodeName]][[thisName]] <<- TRUE
      edgeProperties[[thisName, nextNodeName]] <<-
        list(label=branchName, call=tailcall)
    }
    thisName
  }
  startName <- doWalk(start, "start")
  # Contexts: what environment is each node in?
  contexts <- hashbag()
  contextNodes <- hashbag()
  for (thisNodeName in names(nodes)) {
    thisNode <- nodes[[thisNodeName]]
    context <- environment(nodes[[thisNodeName]])
    if (is.null(contextName <- contains(contexts, context))) {
      contextName <- paste0("ctx.", thisNodeName)
      assert(!exists(contextName, envir=contexts))
      #cat("context: ", contextName, "\n")
      contexts[[contextName]][[thisNodeName]] <- thisNodeName
    }
    #does the node have a name in this context?
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
  # what storage used in those contexts?
  contextProperties <- hashbag()
  list(nodes=nodes,
       edges=edges,
       reverseEdges=reverseEdges,
       contexts=contexts,
       nodeProperties=nodeProperties,
       edgeProperties=edgeProperties,
       start=startName,
       return=returnName,
       stop=stopName)
}


## make_dot
# Code for outputting a DOT file given data collected above
quoted <- function(name) paste0('"', name, '"')
block <- function(block) function(name, ...) {
  c(paste0(block, " ", quoted(name), " { "), paste0("  ", c(...)), "}")}
group <- function(items) paste0("{", paste0(quoted(items), collapse=" "), "}")
attrs <- function(...) {
  x = do.call("c", list(...))
  if(length(x) > 0)
    paste0("[", paste0(names(x), "=", quoted(x), collapse=", "), "]")
  else character(0)}
nodeAttrs <- function(nodeGraph, nodeName) {
  label <- nodeGraph$nodeProperties[[nodeName]]$localName %||% ""
  c(switch(label,
           "R_"=c(label=deparse(expr(environment(nodeGraph$nodes[[nodeName]])$x)),
                  fontname="DejaVu Sans Mono", style="filled"),
           ";"=c(shape="point", color="black", label=";"),
           c(label=label)),
    switch(nodeName,
           START=c(shape="circle", color="darkgreen", fontcolor="white"),
           STOP=c(shape="octagon", color="darkred", fontcolor="white"),
           RETURN=c(shape="doublecircle", size=2),
           c())
  )
}
node <- function(nodeGraph, nodeName) {
  paste(quoted(nodeName),
        attrs(nodeAttrs(nodeGraph, nodeName)))}
nodes <- function(nodeGraph, nodes) {
  concat(lapply(nodes, function(n) node(nodeGraph,nodes)))}
subgraph <- block("subgraph")
subgraphs <- function(nodeGraph) {
  concat(lapply(
    sort(names(nodeGraph$contexts)),
    function(sgName) {
      subgraph(sgName,
               nodes(nodeGraph,
                     sort(names(nodeGraph$contexts[[sgName]]))))}))}
edgeAttrs <- function(nodeGraph, from, to) {
  # edge properties.
  # bolder if carrying a value
  # outgoing label.
  props <- nodeGraph$edgeProperties[[from, to]]
  c(if(props$label=="cont") c(taillabel="") else c(taillabel=props$label),
    if(length(props$call) > 1) #tailcall carries value
      c(color="blue", penwidth="2", arrowhead="normal", arrowtail="normal")
    else c(color="gray", penwidth="1"))}
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
defaults <- function(type, ...) {
  paste(type, attrs(c(...)))}
make_dot <- function(nodeGraph) {
  digraph("G",
          defaults("edge", arrowhead="normal", arrowsize=1),
          defaults("node", style="rounded,filled", shape="box"),
          subgraphs(nodeGraph),
          edges(nodeGraph)
  )
}
