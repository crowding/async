
#' @export
getEntry <- function(x) UseMethod("getEntry")

## environment(genprimes$nextElem)
## ls(environment(genprimes$nextElem)) # make_generator closure
## environment(environment(genprimes$nextElem)$pump) # make_pump closure
## ls(environment(environment(genprimes$nextElem)$pump)) # here's all the pump state
## entry <- environment(environment(genprimes$nextElem)$pump)$cont # the entry point, it's an R()

#' @exportS3Method
getEntry.generator <- function(x) environment(environment(genprimes$nextElem)$pump)$cont

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
  collect_head <- function(expr, inTail) {
    switch(mode(expr),
           call= collect_call(expr, inTail=FALSE),
           character=,
           name=c(if(call) c(call=as.character(expr)),
                  if(inTail && tail) c(tail=as.character(expr))),
           character(0))
  }
  collect_call <- function(expr, inTail) {
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
  collect_arg <- function(expr, inTail)
    switch(mode(expr),
           call=collect_call(expr, inTail),
           name=if(var) c(var=as.character(expr)) else character(0),
           character(0))
  collect_store <- function(dest)
    switch(mode(dest),
           call=collect_store(dest[[2]]),
           character=dest,
           name=as.character(dest),
           character(0))
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

# collect all nodes and edges and give them names,
# nodes being named according to the sequence of branches.
# Collect other interesting information about the nodes.
walk <- function(start, name="start") {
  iter <- icount()
  nodes <- new.env(parent=emptyenv())
  nodeProperties <- new.env(parent=emptyenv())
  edges <- new.env(parent=emptyenv())
  edgeProperties <- new.env(parent=emptyenv())
  doWalk <- function(thisNode, path) {
    if (!is.null(thisName <- contains(nodes, thisNode)))
      return(thisName)
    thisName <- condense.name(path) #don't think make_unique is required
    assert(!exists(thisName, envir=nodes))
    nodes[[thisName]] <- thisNode
    edges[[thisName]] <- new.env(parent=emptyenv())
    tails <- all_names(body(thisNode), "tailcall")
    print(list(node=thisNode, tails=tails))
    for (tailcall in tails) {
      branchName <- as.character(tailcall[[1]])
      nextNode <- get(branchName, envir=environment(thisNode))
      nextNodeName <- doWalk(nextNode, c(path, branchName))
      cat(sprintf("%s:%s -> %s\n", thisName, branchName, nextNodeName))
      edges[[thisName]][[branchName]] <- nextNodeName
      edgeProperties[[paste0(thisName, "~", nextNodeName)]] <- list(
        from=thisName, label=branchName, to=nextNodeName)
    }
    thisName
  }
  startName <- doWalk(start, name)
  # what environment is each node in?
  contexts <- new.env(parent=emptyenv())
  for (thisNodeName in names(nodes)) {
    nodeProperties[[thisNodeName]] <- new.env(parent=emptyenv())
    thisNode <- nodes[[thisNodeName]]
    context <- environment(nodes[[thisNodeName]])
    if (is.null(contextName <- contains(contexts, context))) {
      contextName <- paste0("ctx.", thisNodeName)
      assert(!exists(contextName, envir=contexts))
      cat("context: ", contextName, "\n")
      contexts[[contextName]] <- new.env()
    }
    contexts[[contextName]][[thisNodeName]] <- thisNodeName
    #does the node have a name in this context?
    nodeProperties[[thisNodeName]]$localName <- character(0)
    for (nm in names(context)) {
      #watch out for unforced args like ifnotfound=stop("Not found")
      if (nm != "..." && is_forced_(nm, context)) {
        if (identical(context[[nm]], thisNode)) {
          cat(thisNodeName, " is called ", nm, "\n")
          nodeProperties[[thisNodeName]]$localName <- nm
        }
      }
    }
    # is this a special node?
    nodeProperties[[thisNodeName]]$is_R <-
      exists("_context", environment(thisNode)) &&
         environment(thisNode)$`_context` == "R"
  }
  # what storage used in those contexts?
  contextProperties <- new.env(parent=emptyenv())
  list(nodes=nodes, edges=edges, contexts=contexts,
       start=startName, nodeProperties=nodeProperties,
       edgeProperties=edgeProperties)
}

`%||%` <- function(x, y) if (length(x) == 0) y else x
quoted <- function(name) paste0('"', name, '"')
block <- function(block) function(name, ...) {
  c(paste0(block, " ", quoted(name), " { "), paste0("  ", c(...)), "}")}
group <- function(items) paste0("{", paste0(quoted(items), collapse=" "), "}")
attrs <- function(...) {
  x = do.call("c", list(...))
  if(length(x) > 0)
    paste0("[", paste0(names(x), "=", quoted(x), collapse=" "), "]")
  else character(0)}
nodeLabel <- function(nodeGraph, nodeName) {
  nodeGraph$nodeProperties[[nodeName]]$localName %||% ""}
node <- function(nodeGraph, nodeName) {
  paste(quoted(nodeName),
        attrs(label=nodeLabel(nodeGraph, nodeName)))}
nodes <- function(nodeGraph, nodes) {
  concat(lapply(nodes, function(n) node(nodeGraph,nodes)))}
subgraph <- block("subgraph")
subgraphs <- function(nodeGraph) {
  concat(lapply(
    names(nodeGraph$contexts),
    function(sgName) {
      subgraph(sgName,
               nodes(nodeGraph,
                     names(nodeGraph$contexts[[sgName]])))}))}
edgeAttrs <- function(nodeGraph, from, to) {
  props <- nodeGraph$edgeProperties[[paste0(from, "~", to)]]
  c(label=if(props$label=="cont") "" else props$branchName)
}
edge <- function(nodeGraph, from, to) {
  # edge properties...
  # bolder if carrying a value
  # outgoing label
  concat(lapply(to, function(edgeTo) {
    paste0(quoted(from), " -> ", quoted(edgeTo), attrs(edgeAttrs(nodeGraph, from, edgeTo)))
  }))}
edges <- function(nodeGraph)
  concat(lapply(names(nodeGraph$edges),
                function(x) edge(nodeGraph, x,
                                 as.character(as.list(nodeGraph$edges[[x]])))))
graph <- block("graph")
digraph <- block("digraph")

make_dot <- function(nodeGraph) {
  digraph("G",
    subgraphs(nodeGraph),
    edges(nodeGraph)
  )
}
