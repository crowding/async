# Utility functions, internal consistency checks used in testing

`%is%` <- testthat::expect_equal

mock_channel <- function(...) {
  e <- NULL
  ch <- channel(function(emit, reject, close) {
    e <<- environment()
  }, ...)
  ch$emit <- e$emit
  ch$reject <- e$reject
  ch$close <- e$close
  ch
}

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

expect_properly_munged <- function(graph, gc) {
  graphc <- walk(gc)
  expect_only_one_context(graphc)
  expect_state_pointers_closed(graphc)
  expect_graph_names_are_graphc_locals(graph, graphc)
  expect_edges_isomorphic(graph, graphc)
}


expect_only_one_context <- function(graphc) {
  # Walking gc should have only picked up one context
  expect_equal(length(graphc$contexts), 1)
  env <- as.list(graphc$contexts, all.names=TRUE)[[1]]
  con <- names(graphc$contexts)
  # all nodes are closed in that context
  envs <- lapply(as.list(graphc$contextNodes[[con]], all.names=TRUE),
                 \(nod) environment(graphc$nodes[[nod]]))
  cmp <- envs; cmp[] <- list(env)
  expect_identical(cmp, envs)
}

expect_state_pointers_closed <- function(graphc) {
  # function pointers in "state" "tailcall" roles point to functions
  # in the same context.
  con <- names(graphc$contexts)
  env <- as.list(graphc$contexts, all.names=TRUE)[[1]]
  for (var in sort(unique(c(graphc$contextProperties[[con]][["read"]],
                       graphc$contextProperties[[con]][["store"]])))) {
    val <- graphc$contexts[[con]][[var]]
    if (is.function(val) && !is.quotation(val) && !is.null(body(val))) {
      #is it tailcalled?
      if (var %in% graphc$contextProperties[[con]][["tail"]]) {
        expect_identical(environment(val), env)
      } else {
        if (!is_child_of(environment(val), env)) {
          # cat("a non-tailcall, ", var, ", \n") #Hmm.
          # Functions that have been munged/translated, as well as
          # functions that should not be translated, should not
          # be under the async package namespace.
          # Unless they should, because they are callbacks to a
          # stream constructor, which _is_ under async, but it
          # should not have a visible .contextName
          expect_false(exists(".contextName", environment(val), inherits=TRUE))
          e <- environment(graphc$contexts[[con]][[var]])
          expect_false(exists(".contextName", e, inherits=TRUE))
          expect_false(exists(".packageName", e, inherits=FALSE))
          expect_false(exists(".__NAMESPACE__.", e, inherits=FALSE))
          # actually this happens now, because `channel` is in async
          # and the channel callbacks are replaceable vars:
          #expect_false(is_child_of(e, getNamespace("async")))
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
                     graphc$nodeProperties[[nodeName]][["localName"]],
                   "")
  #if (!setequal(unname(cnodes), names(graph$nodes))) browser()
  expect_setequal(unname(cnodes), names(graph$nodes))
}

expect_edges_isomorphic <- function(graph, graphc) {
  # Also using the property that node names assigned in walk become local names
  # after compilation
  fromcs <- names(graphc$nodeEdgeProperties)
  fromclocals <- (fromcs
    |> vapply( \(nodeName)graphc$nodeProperties[[nodeName]][["localName"]], "" ))
  edgecs <- (structure(fromcs, names=paste0(fromclocals, "->"))
    |> lapply(function(fromc) {
      toclocals <- names(graphc$nodeEdgeProperties[[fromc]])
      tocs <- toclocals |> vapply(
        \(local) graphc$nodeEdgeProperties[[fromc]][[local]]$to, "")
      structure(tocs, names=toclocals)
    })
    |> c(recursive=TRUE)
  )

  froms <- names(graph$nodeEdgeProperties)
  edges <- (
    structure(froms, names=paste0(froms, "->"))
    |> lapply( \(from) {
      tos <- as.list(graph$nodeEdgeProperties[[from]], all.names=TRUE)
      if (length(tos) > 0) {
        (tos
          |> vapply(\(x) structure(names=x$to, x$to), "")
          |> (\(.)structure(names(.), names=.))())
      } else list()
    })
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

expect_rejects_with <- function(prom, expected, trigger=NULL, test=expect_match) {
  nonce <- function() NULL
  val <- nonce
  then(prom, onRejected=function(val) val <<- val, onFulfilled=stop)
  force(trigger)
  wait_for_it()
  if (identical(val, nonce)) stop("Promise did not reject")
  test(conditionMessage(val), expected)
}

strrev <- function(x)
  vapply(strsplit(x, ""),
         \(x) paste0(rev(x), collapse=""),
         "")

#' @import testthat
expect_emits <- function(channel, expected, trigger=NULL, test=expect_equal) {
  nonce <- function() NULL
  val <- nonce
  nextThen(channel, onNext=function(value) val <<- value,
           onError=function(err) stop(err),
           onClose=function() stop("Unexpected channel closing"))
  force(trigger)
  wait_for_it()
  if (identical(val, nonce)) stop("Channel did not emit a value")
  test(val, expected)
}

#' @import testthat
expect_channel_rejects <-
  function(channel, expected, trigger=NULL, test=expect_match) {
  nonce <- function() NULL
  val <- nonce
  nextThen(channel,
           onNext = function(value) stop("Expected channel error, got value"),
           onError = function(value) val <<- value,
           onClose = function() stop("Unexpected finish"))
  force(trigger)
  wait_for_it()
  if (identical(val, nonce)) stop("Channel did not reject")
  test(conditionMessage(val), expected)
}

#' @import testthat
expect_channel_closes <- function(channel, trigger=NULL) {
  finished <- FALSE
  nextThen(channel,
           onNext = function(value) stop("Expected channel finish, got value"),
           onError = function(value) stop(value),
           onClose = function() finished <<- TRUE)
  force(trigger)
  wait_for_it()
  expect_true(finished)
}

traceBinding <- function(name, value,
                         prefix=get(".contextName", envir),
                         envir=caller()) {
    makeActiveBinding(
      name,
      function(new) if (missing(new)) value else {
        value <<- new
        tryCatch(
        {
          if(options$verbose) trace_(paste0(prefix, ": ", name, " <- ", as.character(new), "\n"))
        },
          error=function(err) {print(err); browser()}
        )
        value
      },
      envir)
}
