munge <- function(# the async/generator to munge
                  g,
                  # What is the destination env?  In the interpreted
                  # form the effective parent environment is captured
                  # in the "R_" objects.  In the compiled form it _will_
                  # be the a child of the base environment that called
                  # gen() (different for each invocation!) So we don't
                  # know yet, so just munge to a new env and set its
                  # parent later.
                  dest.env = new.env(parent = baseenv())) {
  # The graph data structure should give us most info we need.
  graph <- walk(g)

  # move over storage
  # including utility functions like "stop" and "trace"
  # (? should be deduped/ included in graph?)
  for (contextName in names(graph$contexts)) {
    context <- graph$contexts[[contextName]]
    contextVars <- graph$contextProperties[[contextName]]$vars

    # collect the translated names of variables and nodes
    varTranslations <- structure(
      as.character(paste0("..", contextName, ".", contextVars, recycle0=TRUE)),
      names=contextVars)
    callTranslations <- concat(lapply(
      names(graph$contextNodes[[contextName]]),
      function(nodeName) {
        nodeEdgeProperties <- as.list(
          graph$edgeProperties[[nodeName]], all.names=TRUE)
        structure(as.list(names(nodeEdgeProperties)),
          names=vapply(nodeEdgeProperties, function(x)x$label, ""))
      }))
    translations <-
      as.environment(c(lapply(callTranslations, as.name),
                       lapply(varTranslations, as.name)))

    # transfer each binding in this context
    f <- is_forced_(names(varTranslations), context)
    if (any(!f)) {
      stop("Unforced arguments found in munging: ",
           paste(names(f)[[!f]], collapse=", "))
    }
    for(i in seq_along(varTranslations)) {
      # how would this handle a "state" variable? Guess we'll find out next....
      # cat(paste0(varTranslations[[i]], " <- ",
      #            contextName, " :: ", names(varTranslations)[[i]], "\n"))
      dest.env[[varTranslations[[i]]]] <- context[[names(varTranslations)[[i]]]]
    }

    # translate each node
    for (nodeName in names(graph$contextNodes[[contextName]])) {
      node <- graph$nodes[[nodeName]]
      nodeBody <- body(node)
      # Translate the node's exits.
      targets <- names(graph$edgeProperties[[nodeName]]) %||% list()
      labels <- vapply(targets, function(target)
        graph$edgeProperties[[nodeName]][[target]]$label, "")
      translatedBody <- substituteDirect(nodeBody, translations)
      # cat(paste0(nodeName, " <- ", contextName, "::", "\n"))
      dest.env[[nodeName]] <-
        function_(formals(node), translatedBody, dest.env)
    }
  }
  dest.env
}

dedupe <- function(x) x[!duplicated(x)] # keeps labels
