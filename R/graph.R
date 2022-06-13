## graph.R
# Code for outputting a Graphviz DOT file given data collected in walk.R
quoted <- function(name) paste0('"', gsub('(["])', "\\\\\\1", name, ""), '"')
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
                          collapse="\\l"),
             fontname="DejaVu Sans Mono Bold", style="filled",
             fontcolor="lightgreen", fontsize=13, color="gray20", labeljust="l"),
      ";"=c(shape="circle", style="filled", color="gray70",
            fixedsize="true",
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
              paste0("<", varNames, ">", varNames, collapse="|")),
            fontsize=11,
            fontname="DevaVu Sans Mono Bold", margin=0.08)),
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
                    attrs(penwidth=1, color="slateblue3",
                          arrowsize=0.5, arrowhead="dot")),
            if (length(stores) > 0)
              paste(store(node, paste0(context, "_var"), stores),
                    attrs(penwidth=1, color="orange3",
                          arrowsize=0.5, arrowhead="dot"))
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
          subgraph(paste0("cluster1_", sgName), # dummy...
                   props(margin=6, style="invis"),
            subgraph(paste0("cluster_", sgName),
                   props(label="", shape="box", style="dashed,rounded",
                         #rank="same",
                         margin=12, penwidth=3, color="gray70"),
                   nodes(nodeGraph,
                         sort(names(nodeGraph$contextNodes[[sgName]]))),
                   storage(nodeGraph, sgName)))
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
    if (length(props$call) > 1) { #"special" or trampolined tailcalls
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
    props(bgcolor="lightgray", margin=0, pad=1,
          nodesep=0.3, ranksep=0.4, newrank="true", # packmode="graph",
          labeljust="l", fontname="DejaVu Sans Mono Book", rankdir="TB",
          fontsize=14),
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
                                      collapse="\\l")))),
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
