# Utility functions, stuff used in testing

`%is%` <- testthat::expect_equal

mock_promise <- function() {
  resolve <- NULL
  reject <- NULL
  value <- NULL
  err <- NULL
  state <- "pending"
  p <- promise(function(resolve, reject) {resolve <<- resolve; reject <<- reject})
  then(p, function(x) {value <<- x; state <<- "resolved"},
       function(e) {err <<- e; state <<- "rejected"} )
  p$resolve <- resolve
  p$reject <- reject
  p$state <- function() switch(state,
                               pending = list(state=state),
                               resolved = list(state=state, value=value),
                               rejected = list(state=state, err=err))
  structure(p, class=c("promise", "mock_promise"))
}

# Block until all pending later tasks have executed
wait_for_it <- function(timeout = 30) {
  start <- Sys.time()
  while (!later::loop_empty()) {
    if (difftime(Sys.time(), start, units = "secs") > timeout) {
      stop("Waited too long")
    }
    later::run_now()
    Sys.sleep(0.01)
  }
}

expect_properly_munged <- function(g, gc) {
  graph <- walk(g)
  graphc <- walk(gc)
  expect_only_one_context(graphc)
  expect_state_pointers_closed(graphc)
  expect_graph_names_are_graphc_locals(graph, graphc)
  expect_edges_isomorphic(graph, graphc)
}

expect_only_one_context <- function(graphc) {
  # Walking gc should have only picked up one context
  expect_equal(length(graphc$contexts), 1)
  env <- as.list(graphc$contexts)[[1]]
  con <- names(graphc$contexts)
  # all nodes are closed in that context
  for (nod in as.list(graphc$contextNodes[[con]])) {
    expect_identical(environment(graphc$nodes[[nod]]), env)
  }
}

expect_state_pointers_closed <- function(graphc) {
  # function pointers in "state" "tailcall" roles point to functions
  # in the same context.
  con <- names(graphc$contexts)
  env <- as.list(graphc$contexts)[[1]]
  for (var in unique(c(graphc$contextProperties[[con, "read"]],
                       graphc$contextProperties[[con, "store"]]))) {
    val <- graphc$contexts[[con]][[var]]
    if (is.function(val) && !is.quotation(val) && !is.null(body(val))) {
      #is it tailcalled?
      if (var %in% graphc$contextProperties[[con, "tail"]]) {
        expect_identical(environment(val), env)
      } else {
        if (!is_child_of(environment(val), env)) {
          cat("a non-tailcall, ", var, ", \n") #Hmm.
          # Functions that have been munged/translated, as well as
          # functions that should not be translated, should not
          # be under the async package namespace.
          e <- environment(graphc$contexts[[con]][[var]])
          expect_false(is_child_of(e, getNamespace("async")))
        }
      }
    }
  }
}

is_child_of <- function(child, parent) {
  while(!identical(child, emptyenv())) {
    if (identical(child, parent)) return(TRUE)
    child <- parent.env(child)
  }
  return(FALSE)
}

expect_graph_names_are_graphc_locals <- function(graph, graphc) {
  # the names assigned walking the graph should become local names of
  # graphc.
  cnodes <- vapply(names(graphc$nodes),
                   function(nodeName)
                     graphc$nodeProperties[[nodeName, "localName"]],
                   "")
  if (!setequal(unname(cnodes), names(graph$nodes))) browser()
  expect_setequal(unname(cnodes), names(graph$nodes))
}

expect_edges_isomorphic <- function(graph, graphc) {
  # Also using the property that node names assigned in walk become local names
  # after compilation
  fromcs <- names(graphc$nodeEdgeProperties)
  fromclocals <- (fromcs
    |> vapply( \(nodeName)graphc$nodeProperties[[nodeName, "localName"]], "" ))
  edgecs <- (structure(fromcs, names=paste0(fromclocals, "->"))
    |> lapply(function(fromc) {
      toclocals <- names(graphc$nodeEdgeProperties[[fromc]])
      tocs <- toclocals |> vapply(
        \(local) graphc$nodeEdgeProperties[[fromc, local]]$to, "")
      structure(tocs, names=toclocals)
    })
    |> c(recursive=TRUE)
  )

  froms <- names(graph$nodeEdgeProperties)
  edges <- (structure(froms, names=paste0(froms, "->"))
    |> lapply( \(from)
      as.list(graph$nodeEdgeProperties[[from]])
      |> vapply(\(x) x$to, "")
      |> (\(.)structure(names(.), names=.))())
    |> c(recursive=TRUE)
  )

  expect_setequal(names(edges), names(edgecs))
}

expect_resolves_with <- function(prom, expected, trigger=NULL, test=expect_equal) {
  nonce <- function() NULL
  val <- nonce
  then(prom, onFulfilled=function(val) val <<- val, onRejected=stop)
  force(trigger)
  wait_for_it()
  if (identical(val, nonce)) stop("Promise did not resolve")
  test(val, expected)
  val
}
