## graph.R
# Code for outputting a Graphviz DOT file given data collected in walk.R

make_dot <- function(nodeGraph,
                     envs=TRUE,
                     vars=TRUE,
                     handlers=TRUE) {
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
             fontcolor="lightgreen", fontsize=13, color="gray20",
             labeljust="l", nojustify="true"),
      ";"=c(shape="circle", style="filled", color="gray70",
            fixedsize="true",
            width=0.25, height=0.25, label=";"),
      c(label=label, style="filled,rounded", color="gray70")),
      switch(
        nodeName,
        entry=,
        START=c(shape="doubleoctagon", color="darkgreen", style="filled",
                fontcolor="lightgreen", margin="0,0", fixedsize="false",
                pos="1,1"
                ),
        stop_=,
        reject=,
        STOP=c(shape="doubleoctagon", color="darkred", style="filled",
               fontcolor="pink",  margin="0,0", fixedsize="false"),
        return=,
        resolve=,
        RETURN=c(shape="doubleoctagon", color="darkblue", style="filled",
                 fontcolor="lightblue", margin="0,0", fixedsize="false")))}
  node <- function(nodeGraph, nodeName) {
    paste(quoted(nodeName),
          attrs(nodeAttrs(nodeGraph, nodeName)))}
  nodes <- function(nodeGraph, nodes) {
    concat(lapply(nodes, function(n) node(nodeGraph,n)))}
  read <- function(from, field, to)
    paste0(quoted(from), ":", quoted(field), " -> ", quoted(to))
  store <- function(from, to, field)
    paste0(quoted(from), " -> ", quoted(to), ":", quoted(field))
  storage <- function(nodeGraph, context) {
    # make a record node for context storage
    # they say record-based nodes are obsoleted by html-style labels
    # but I'll start with this...
    varNames <- union(nodeGraph$contextProperties[[context]]$read,
                      union(nodeGraph$contextProperties[[context]]$store,
                            nodeGraph$contextProperties[[context]]$utility))
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
            reads <- nodeGraph$nodeProperties[[node]]$read
            stores <- nodeGraph$nodeProperties[[node]]$store
            calls <- nodeGraph$nodeProperties[[node]]$utility
            ## edges for the storage
            c(
              if (length(reads) > 0)
                paste(read(paste0(context, "_var"), reads, node),
                      attrs(penwidth=1, color="slateblue3",
                            arrowsize=0.5, arrowhead="dot",
                            concentrate="true", constrain="false")),
              if (length(stores) > 0)
                paste(store(node, paste0(context, "_var"), stores),
                      attrs(penwidth=1, color="orange3",
                            arrowsize=0.5, arrowhead="dot",
                            concentrate="true", constrain="false")),
              if (length(calls) > 0)
                paste(read(paste0(context, "_var"), calls, node),
                      attrs(penwidth=1, color="darkgreen",
                            arrowsize=0.5, arrowhead="dot",
                            concentrate="true", constrain="false"))
            )})))}}
  subgraph <- block("subgraph")
  subgraphs <- function(nodeGraph) {
    concat(
      lapply(
        sort(names(nodeGraph$contexts)),
        function(sgName) {
          nodeNames <- names(nodeGraph$contextNodes[[sgName]])
          # draw a box IF there are multiple nodes OR there is storage.
          # but NOT if it's a "passthrough R code" node (which does have
          # read-only storage?)
          label <- nodeGraph$nodeProperties[[nodeNames[[1]]]]$localName %||% ""
          if ((
            ( (length(nodeNames) > 1)
              || (length(nodeGraph$contextProperties[[sgName]]$vars) > 0) )
            && ( label != "R_" )
          )) {
            contents <- c(
              nodes(nodeGraph, sort(names(nodeGraph$contextNodes[[sgName]]))),
              if(vars) storage(nodeGraph, sgName))
            if (envs)
              subgraph(paste0("cluster1_", sgName), # dummy...
                       props(margin=6, style="invis"),
                       subgraph(paste0("cluster_", sgName),
                                props(label="", shape="box", style="rounded",
                                      bgcolor="gray85",
                                      #rank="same",
                                      margin=12, penwidth=1, color="gray75"),
                                contents))
            else contents
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
    c(if (props$localName=="cont" || length(nodeGraph$edgeProperties[[from]]) <= 1)
      c(taillabel="") else c(taillabel=props$localName),
      if (length(props$call[[1]]) > 1)
        c(color="black") else c(color="gray50"),
      switch(props[["type"]],
             tail=c(arrowhead="normal", penwidth="2", concentrate="true"),
             tramp=c(style="dashed", penwidth="2.5",
                          constrain="false", concentrate="false"),
             hand=c(penwidth="0.75", arrowhead="dot", arrowsize="0.3",
                    concentrate="true", constrain="false"),
             stop("unknown edge type?")),
      ## if (identical(nodeGraph$nodeProperties[[to]]$localName, ";"))
      ##   c(arrowhead="none"),
      if (props$type=="trampoline") { #"special" or trampolined tailcalls
        c(arrowhead="odot", taillabel=" ", labelangle=0, fontsize=15, arrowsize=2.25,
          labeldistance=.9, fontcolor="blue",
          switch(as.character(props$call[[2]][[1]]),
                 ret=c(headlabel="⮍"),
                 yield=,
                 pause=c(headlabel="⏸",labeldistance=0.8),
                 windup=c(headlabel="⤽", fontsize=20),
                 unwind=c(headlabel="⤼", fontsize=20)))
      }
      )}
  edge <- function(nodeGraph, from, to) {
    concat(lapply(to, function(edgeTo) {
      edge <- nodeGraph$edgeProperties[[from, to]]
      if (edge[["type"]] != "hand" ||
            ((edge[["type"]] == "hand") && handlers)) {
        paste(quoted(from), "->", quoted(edgeTo),
              attrs(edgeAttrs(nodeGraph, from, to)))}}))}
  edges <- function(nodeGraph) {
    concat(lapply(all_indices(nodeGraph$edgeProperties),
                  function(i) edge(nodeGraph, i[1], i[2])))}
  graph <- block("graph")
  digraph <- block("digraph")

  props <- function(...) {
    x = c(...)
    paste(names(x), "=", quoted(x))}
  defaults <- function(type, ...) {
    paste(type, attrs(c(...)))}

  digraph(
    "G",
    props(bgcolor="lightgray", margin=0, pad=0.25, concentrate="false",
          nodesep=0.3, ranksep=0.4, newrank="true",
          clusterrank="local", packmode="clust",
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

#' Draw a graph representation of a generator.
#'
#' `drawGraph` will traverse the objects representing a
#' [generator][gen] or [async] and render a graph of its structure
#' using Graphviz (if it is installed.)
#'
#' `drawGraph` will write a Graphviz DOT format file describing the
#' given [generator][gen] or [async]/await block. The graph shows the
#' generator as a state machine with nodes that connect to each other.
#'
#' If `type` is something other than `dot` `drawGraph will then try to
#' invoke Graphviz  DOT to turn the graph description into an image
#' file.
#'
#' The green octagonal node is where evaluation starts, while red
#' "stop" and blue "return" are where it ends. Nodes in green type on
#' dark background show code that runs in the host language
#' unmodified; gray nodes implement control flow. Dark arrows carry a
#' value; gray edges carry no value. A "semicolon" node receives a
#' value and discards it.
#'
#' Some nodes share a context with other nodes, shown by an enclosing
#' box. Contexts can have state variables, shown as a rectangular
#' record; orange edges from functions to variables represent writes;
#' blue edges represent reads.
#'
#' Dashed edges represent a state transition that goes through a
#' trampoline handler. Dashed edges have a symbol representing the
#' type of trampoline; (⏸) for await/yield; (⤽) or (⤼) to wind on or
#' off an exception handler; (⮍) for a plain trampoline with no side
#' effects (done once per loop, to avoid overflowing the stack.)
#' Meanwhile, a thin edge connects to the trampoline handler; (so the
#' user-facing "yield" function registers a continuation to the next
#' step but actually calls the generator's yield handler.)
#'
#' @examples
#' randomWalk <- gen({x <- 0; repeat {yield(x); x <- x + rnorm(1)}})
#' \dontrun{
#' makeGraph(randomWalk, "pdf")
#' # writes "randomWalk.dot" and invokes dot to make "randomWalk.pdf"
#'
#' #or, display it in R with the Rgraphviz package:
#' g <- Rgraphviz::agread("randomWalk.dot")
#' Rgraphviz::plot(g)
#' }
#' @param obj A [generator][gen] or [async] object.
#' @param type the output format. If "dot", we will just write a Graphviz
#'   dot file. If another extension like "pdf" or "svg", will write a
#'   DOT file and then attempt to invoke Graphviz `dot` (if it is
#'   available according to [`Sys.which`]) to produce the image.
#' @param basename The base file name. If `basename="X"` and `type="pdf"`
#'   you will end up with two files, `"X.dot"` and `"X.pdf"`.
#' @param envs If `TRUE`, multiple nodes that share the same
#'   environment will be grouped together in clusters.
#' @param vars If `TRUE`, context variables used in each state node
#'   will be included on the graph, with edges indicating reads/stores.
#' @param handlers If `TRUE`, state nodes will have thin edges
#'   connecting to their trampoline handlers, in addition to the dashed edges
#'   connecting to the next transition.
#' @param dot Optional path the the `dot` executable.
#' @param dotfile Optionally specify the output DOT file name.
#' @param filename Optionally specify the output picture file name.
#' @return The name of the file that was created.
#' @export
drawGraph <- function(obj,
                      basename=as.character(
                        unless(substitute(obj), is.name,
                               stop("Please specify a base filename"))),
                      type="pdf",
                      envs=TRUE,
                      vars=TRUE,
                      handlers=TRUE,
                      dot=unless(Sys.which("dot"), function(x)x!="",{
                        message("Graphviz was not found; writing DOT file only");
                      }),
                      filename=paste0(basename, ".", type),
                      dotfile=if (type=="dot")
                        filename else paste0(basename, ".dot"))
{
  makeGraph(obj, dotfile, envs=envs, vars=vars, handlers=handlers)
  if (type == "dot" || dot == "") {
    dotfile
  } else {
    result <- system2(dot, c(paste0("-T", type), dotfile), stdout=filename)
    if (result != 0) stop("status", attr(result, "status"), ": ", stderr=
                          attr(result, "errmsg"))
    filename
  }
}

unless <- function(x, filter, or) if(filter(x)) x else or

makeGraph <- function(x, file="", sep="\n", ...) {
  UseMethod("makeGraph")
}

#' @exportS3Method
makeGraph.generator <- function(x, file="", sep="\n", ...) {
  graph <- walk(x)
  cat(make_dot(graph, ...), file=file, sep=sep)
  invisible(graph)
}

#' @exportS3Method
makeGraph.async <- function(x, file="", sep="\n", ...) {
  graph <- walk(x)
  cat(make_dot(graph, ...), file=file, sep=sep)
  invisible(graph)
}
