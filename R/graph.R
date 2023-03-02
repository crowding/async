## graph.R
# Code for outputting a Graphviz DOT file given data collected in walk.R

last <- function(x) x[[length(x)]]

make_dot <- function(nodeGraph,
                     envs=TRUE,
                     vars=TRUE,
                     handlers=TRUE,
                     orphans=TRUE) {
  quoted <- function(name) paste0('"', gsub('(["\\\\])', "\\\\\\1", name, ""), '"')
  block <- function(block) function(name, ...) {
    c(paste0(block, " ", quoted(name), " { "), paste0("  ", c(...)), "}")}
  group <- function(items) paste0("{", paste0(quoted(items), collapse=" "), "}")
  attrs <- function(..., quote=TRUE) {
    x = c(...)
    if(length(x) > 0)
      paste0("[", paste0(names(x), "=", if(quote)quoted(x)else x, collapse=", "), "]")
    else character(0)}
  nodeAttrs <- function(nodeGraph, nodeName) {
    label <- nodeGraph$nodeProperties[[nodeName]]$localName %||% nodeName
    short_label <- last(strsplit(label, "__")[[1]])
    is_handler <- "cont" %in% names(formals(nodeGraph$nodes[[nodeName]]))
    c(if (is_R(nodeGraph$nodes[[nodeName]])) {
      ex <- R_expr(nodeGraph$nodes[[nodeName]])
      c(label=paste0(deparse(ex),
                     collapse="\\l"),
        fontname="DejaVu Sans Mono Bold", style="filled",
        fontcolor="lightgreen", fontsize=13, color="gray20",
        labeljust="l", nojustify="true")
    } else if (short_label == ";") {
      c(shape="circle", style="filled", color="gray70",
        fixedsize="true",
        width=0.25, height=0.25, label=";")
    } else {
      c(label=label, style="filled,rounded", color="gray70")
    },
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
    if (!orphans) {
      # check if node needs to be included
      if (!vars) { #if you include vars, no point trimming the mess
        if (length(nodeGraph$nodeEdgeProperties[[nodeName]]) == 0) {
          if (length(nodeGraph$reverseEdges[[nodeName]]) == 0) {
            return(NULL)
          } else {
            if(!handlers) {
              incoming <- as.list(nodeGraph$reverseEdges[[nodeName]])
              reverseEdgeTypes <- mapply(
                names(incoming), incoming, FUN=\(from, exit) {
                  nodeGraph$nodeEdgeProperties[[from]][[exit]]$type
                },
                SIMPLIFY=TRUE)
              if (all(reverseEdgeTypes == "hand")) {
                return(NULL)
    }}}}}}
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
      safeVarNames <- gsub("([][{}()<>| \\\\])", "\\\\\\1", varNames)
      c(paste(
        quoted(paste0(context, "_", "var")),
        attrs(quote=FALSE,
              shape="record",
              label=paste0("\"{",
                paste0("<", safeVarNames, ">", safeVarNames, collapse="|"), "}\""),
              fontsize=11,
              fontname=quoted("DevaVu Sans Mono Bold"), margin=0.08)),
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
                                props(label=sgName, shape="box", style="rounded",
                                      fontname="DejaVu Sans Bold",
                                      fontcolor="gray50", fontsize=13,
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
  edgeAttrs <- function(nodeGraph, from, local) {
    # edge properties.
    # bolder if carrying a value
    # outgoing label.
    props <- nodeGraph$nodeEdgeProperties[[from]][[local]]
    c(if (local=="cont" || length(nodeGraph$nodeEdgeProperties[[local]]) <= 1)
      c(taillabel="") else c(taillabel=props$localName),
      if (length(props$call[[1]]) > 1)
        c(color="black") else c(color="gray50"),
      switch(props[["type"]],
             tail=c(arrowhead="normal", penwidth="2", concentrate="true"),
             tramp=c(style="dashed", penwidth="2.5",
                     constrain="false", concentrate="false"),
             wind=,
             hand=c(penwidth="0.75", arrowhead="dot", arrowsize="0.3",
                    concentrate="true", constrain="false"),
             stop("unknown edge type?")),
      ## if (identical(nodeGraph$nodeProperties[[to]]$localName, ";"))
      ##   c(arrowhead="none"),
      if (props$type=="tramp" && length(props$call) > 1) {
        #"special" or trampolined tailcalls
        handler <- as.character(props$call[[2]][[1]])
        handler_edge <- c(arrowhead="odot", taillabel=" ", labelangle=0,
                          fontsize=15, arrowsize=2.25,
                          labeldistance=.9, fontcolor="blue")
        switch(as.character(props$call[[2]][[1]]),
               ret=c(handler_edge, headlabel="\u2b8d"),
               yield=,
               pause=c(handler_edge, headlabel="\u23f8", labeldistance=0.8),
               windup=c(handler_edge, headlabel="\u293d", fontsize=20),
               unwind=c(handler_edge, headlabel="\u293c", fontsize=20),
               evl=c(style="solid"),
               c())
      })
  }
  edge <- function(nodeGraph, from, local) {
    concat(lapply(local, function(edgeLocal) {
      edge <- nodeGraph$nodeEdgeProperties[[from]][[local]]
      if (edge[["type"]] != "hand" ||
            ((edge[["type"]] == "hand") && handlers)) {
        paste(quoted(from), "->", quoted(edge$to),
              attrs(edgeAttrs(nodeGraph, from, local)))}}))}
  edges <- function(nodeGraph) {
    concat(lapply(all_indices(nodeGraph$nodeEdgeProperties, levels=2),
                  function(i) {
                    if (length(i) < 2) list()
                    else edge(nodeGraph, i[1], i[2])
                  }))}
  graph <- block("graph")
  digraph <- block("digraph")

  props <- function(...) {
    x = c(...)
    paste(names(x), "=", quoted(x))}
  defaults <- function(type, ...) {
    paste(type, attrs(c(...)))}

  digraph(
    "G",
    props(bgcolor="gray90", margin=0, pad=0.25, concentrate="false",
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

#' Draw a graph representation of a coroutine.
#'
#' `graphAsync` will traverse the objects representing a
#' [generator][gen] or [async] and render a graph of its structure
#' using Graphviz (if it is installed.)
#'
#' `graphAsync` will write a Graphviz DOT format file describing the
#' given [generator][gen] or [async]/await block. The graph shows the
#' generator as a state machine with nodes that connect to each other.
#'
#' If `type` is something other than `dot` `graphAsync will then try to
#' invoke Graphviz `dot` to turn the graph description into an image
#' file.
#'
#' The green octagonal node is where the program starts, while red
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
#' trampoline handler. Dashed edges have a Unicode symbol representing
#' the type of trampoline; (DOUBLE VERTICAL BAR) for await/yield; (TOP
#' ARC ANTICLOCKWISE ARROW WITH PLUS) or (TOP ARC CLOCKWISE ARROW WITH
#' MINUS) to wind on or off an exception handler; (ANTICLOCKWISE
#' TRIANGLE-HEADED BOTTOM U-SHAPED ARROW) for a plain trampoline with
#' no side effects (done once per loop, to avoid overflowing the
#' stack.)  Meanwhile, a thin edge connects to the trampoline handler.
#' (So the user-facing "yield" function registers a continuation to
#' the next step but actually calls the generator's yield handler.)
#'
#' @examples
#' randomWalk <- gen({x <- 0; repeat {yield(x); x <- x + rnorm(1)}})
#' \dontrun{
#' graphAsync(randomWalk, "pdf")
#' # writes "randomWalk.dot" and invokes dot to make "randomWalk.pdf"
#'
#' #or, display it in an R window with the Rgraphviz package:
#' g <- Rgraphviz::agread("randomWalk.dot")
#' Rgraphviz::plot(g)
#' }
#' #Or render an HTML sidget using DiagrammeR:
#' \dontrun{
#' dot <- graphAsync(randomWalk, type="")
#' DiagrammeR::DiagrammeR(paste0(dot, collapse="\n"), type="grViz")
#' }
#' @param obj A [generator][gen], [async] or [stream] object.
#' @param type the output format. If "dot", we will just write a
#'   Graphviz dot file. If another extension like "pdf" or "svg", will
#'   write a DOT file and then attempt to invoke Graphviz `dot` (if it
#'   is available according to [`Sys.which`]) to produce the image.
#'   If `type=""` `graphAsync` will return graphviz DOT language as a
#'   character vector
#' @param basename The base file name. If `basename="X"` and
#'   `type="pdf"` you will end up with two files, `"X.dot"` and
#'   `"X.pdf"`.
#' @param envs If `TRUE`, multiple nodes that share the same
#'   environment will be grouped together in clusters.
#' @param vars If `TRUE`, context variables used in each state node
#'   will be included on the graph, with edges indicating
#'   reads/stores.
#' @param handlers If `TRUE`, state nodes will have thin edges
#'   connecting to trampoline handlers they call, in addition to the
#'   dashed edges connecting to the next transition.
#' @param orphans If `TRUE`, nodes will be included even if there are
#'   no connections to them (this mostly being interface methods and
#'   unused handlers).
#' @param dot Optional path to the `dot` executable.
#' @param dotfile Optionally specify the output DOT file name.
#' @param filename Optionally specify the output picture file name.
#' @param ... Unused.
#' @return If `type=""`, a character vector of DOT source. Else
#'         The name of the file that was created.
#' @export
graphAsync <- function(obj,
                      basename=if (is.name(substitute(obj)))
                        as.character(substitute(obj))
                      else stop("Please specify basename"),
                      type="pdf",
                      ...,
                      envs=TRUE,
                      vars=FALSE,
                      handlers=FALSE,
                      orphans=FALSE,
                      dot=find_dot(),
                      filename=paste0(basename, ".", type),
                      dotfile=if (type=="dot")
                        filename else paste0(basename, ".dot"))
{
  gr <- make_graph(obj, envs=envs, vars=vars, handlers=handlers, orphans=orphans)
  if (type == "") {
    gr
  } else {
    cat(gr, file=dotfile, sep="\n")
    if (type == "dot" || dot == "") {
      dotfile
    } else {
      result <- system2(dot, c(paste0("-T", type), dotfile), stdout=filename)
      if (result != 0) stop("status", attr(result, "status"), ": ",
                            stderr=attr(result, "errmsg"))
      filename
    }
  }
}

find_dot <- function() {
  x <- Sys.which("dot")
  if (x == "")
    if (!on_cran() && !on_ci()) {
      message("Graphviz was not found; writing DOT file only")
      return("")
    }
  x
}

make_graph <- function(x, ...) UseMethod("make_graph")

#' @exportS3Method
make_graph.coroutine <- function(x, ...) {
  graph <- walk(x, forGraph=TRUE)
  make_dot(graph, ...)
}
