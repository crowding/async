# Utility functions, internal consistency checks used in testing

`%is%` <- testthat::expect_equal

mock_channel <- function(...) {
  e <- NULL
  ch <- channel(function(emit, reject, finish) {
    e <<- environment()
  }, ...)
  list(emit=e$emit, reject=e$reject, finish=e$finish, ch=ch)
}

mock_lazy_channel <- function(...) {
  dummy <- \(val) stop("no active request")
  doEmit <- dummy
  doReject <- dummy
  doFinish <- dummy
  args <- list()

  reset <- function() {
    args <<- list()
    doEmit <<- dummy; doReject <<- dummy; doFinish <<- dummy;
  }

  ch <- lazy_channel(function(emit, reject, finish, ...) {
    args <<- list(...)
    doEmit <<- \(val) {reset(); emit(val)}
    doReject <<- \(err, ...) {reset(); reject(err, ...)}
    doFinish <<- \() {reset(); finish()}
  }, ...)

  list(emit=\(val)doEmit(val), reject=\(e)doReject(e),
       finish=\()doFinish(), currentArgs=\()args, ch=ch)
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
    Sys.sleep(0.000000000001)
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

channel_result <- function(channel, trigger) {
  response <- NULL
  value <- NULL
  finished <- FALSE
  nextThen(channel,
           onNext = function(value) {
             response <<- "next"; value <<- value},
           onError = function(value) {
             response <<- "error"; value <<- value},
           onFinish = function()
             response <<- "finish")
  force(trigger)
  wait_for_it()
  list(response, value)
}

#' @import testthat
expect_emits <- function(channel, expected, trigger=NULL, test=expect_equal) {
  result <- channel_result(channel, trigger)
  expect_equal(result[[1]], "next")
  test(result[[2]], expected)
}

#' @import testthat
expect_channel_rejects <-
  function(channel, expected, trigger=NULL, test=expect_match) {
  result <- channel_result(channel, trigger)
  expect_equal(result[[1]], "error")
  test(conditionMessage(result[[2]]), expected)
}

#' @import testthat
expect_channel_finishes <- function(channel, trigger=NULL) {
  result <- channel_result(channel, trigger)
  expect_equal(result[[1]], "finish")
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
          if(getOption("async.verbose")) trace_(paste0(prefix, ": ", name, " <- ", as.character(new), "\n"))
        },
          error=function(err) {print(err); browser()}
        )
        value
      },
      envir)
}

on_cran <- function() !identical(Sys.getenv("NOT_CRAN"), "true")

on_ci <- function() { isTRUE(as.logical(Sys.getenv("CI"))) }

setCompileLevelFromFn <- function(fn) {
  level <- stringr::str_match(getSrcFilename(fn),
                              "level([-+0-9]+)")[,-1]
  if (!nullish(level) && !is.na(level)) {
    options(async.compileLevel = as.numeric(level),
            async.paranoid=TRUE)
  } else {
    options(async.compileLevel = 0,
            async.paranoid=FALSE)
  }
}

stop_unused <- function() NULL

# from package "vctrs"
s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  register <- function(...) {
    envir <- asNamespace(package)

    # Refresh the method each time, it might have been updated by
    # `devtools::load_all()`
    method_fn <- get_method(method)
    stopifnot(is.function(method_fn))


    # Only register if generic can be accessed
    if (exists(generic, envir)) {
      registerS3method(generic, class, method_fn, envir = envir)
    } else if (identical(Sys.getenv("NOT_CRAN"), "true")) {
      # This message is only shown to developers using devtools.
      # Do you need to update %s to the latest version?
      warning(sprintf(
        "Can't find generic `%s` in package %s to register S3 method.",
        generic,
        package
      ))
    }
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(packageEvent(package, "onLoad"), function(...) {
    register()
  })

  # For compatibility with R < 4.0 where base isn't locked
  is_sealed <- function(pkg) {
    identical(pkg, "base") || environmentIsLocked(asNamespace(pkg))
  }

  # Avoid registration failures during loading (pkgload or regular).
  # Check that environment is locked because the registering package
  # might be a dependency of the package that exports the generic. In
  # that case, the exports (and the generic) might not be populated
  # yet (#1225).
  if (isNamespaceLoaded(package) && is_sealed(package)) {
    register()
  }

  invisible()
}
