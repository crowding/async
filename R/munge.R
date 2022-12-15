trace <- function(...) cat(..., sep="")
trace <- function(...) NULL

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
    trace("Context: ", contextName, "\n")
    context <- graph$contexts[[contextName]]
    contextVars <- graph$contextProperties[[contextName]]$read
    utils <- setdiff(graph$contextProperties[[contextName]]$utility, contextVars)
    # collect the translated names of variables and nodes
    varTranslations <- structure(
      as.character(paste0(contextName, contextVars, recycle0=TRUE)),
      names=contextVars)
    callTranslations <- concat(lapply(
      names(graph$contextNodes[[contextName]]),
      function(nodeName) {
        props <- as.list(graph$edgeProperties[[nodeName]])
        structure(names(props) %||% character(0),
                  names=vapply(props, function(x) x$label, ""))
      }))
    utilTranslations <- structure(paste0(contextName, utils, recycle0=TRUE),
                                  names=utils)

    translations <-
      as.environment(c(lapply(callTranslations, as.name),
                       lapply(varTranslations, as.name),
                       lapply(utilTranslations, as.name)))

    trace(" Moving nodes:\n")
    for (nodeName in names(graph$contextNodes[[contextName]])) {
      # nodeName is the translated node name that walk() came up with
      node <- graph$nodes[[nodeName]]
      nodeBody <- body(node)
      # Translate the exits and storage used by the node
      translatedBody <- substituteDirect(nodeBody, translations)
      trace("   Node: `", contextName, "`$`",
          graph$nodeProperties[[nodeName]]$localName,
          "` -> `", nodeName, "`\n")
      dest.env[[nodeName]] <-
        function_(formals(node), translatedBody, dest.env)
    }

    if (length(utilTranslations) > 0) {
      trace(" Moving utils:\n")
      for (fnam in names(utilTranslations)) {
        func <- graph$contexts[[contextName]][[fnam]]
        trace("   Function: `", contextName, "`$`", fnam,
            "` -> `", utilTranslations[[fnam]], "`\n")
        if(FALSE) {
          #translate the function?
          translatedBody <- substituteDirect(body(func), translations)
          dest.env[[utilTranslations[[fnam]]]] <-
            function_(formals(func), translatedBody, dest.env)
        } else {
          #copy direct, Don't translate;
          #fns marked "utility" are "bound locally" but not "defined locally."
          dest.env[[utilTranslations[[fnam]]]] <- func
        }
      }
    }

    # transfer each binding in this context
    if (length(varTranslations) > 0) {
      trace(" Moving data:\n")
      f <- is_forced_(names(varTranslations), context)
      if (any(!f)) {
        stop("Unforced arguments found in munging: ",
             paste(names(f)[[!f]], collapse=", "))
      }
      for (varName in names(varTranslations)) {
        newName <- varTranslations[[varName]]
        move_value(graph, contextName, varName, dest.env, newName,
                   varTranslations, callTranslations)
      }
    }

  }
  dest.env
}

move_value <- function(graph, contextName, varName, dest.env, newName,
                       varTranslations, callTranslations) {
  value <- graph$contexts[[contextName]][[varName]]
  UseMethod("move_value", value)
}

move_value.quotation <- function(graph, contextName, varName, dest.env, newName,
                                 varTranslations, callTranslations) {
  # quotations can be of mode "function," but should be moved directly
  # _without_ modifying their environment.
  dest.env[[varTranslations[[varName]]]] <-
    graph$contexts[[contextName]][[varName]]
}

move_value.function <- function(graph, contextName, varName, dest.env, newName,
                                varTranslations, callTranslations) {

  written <- varName %in% graph$contextProperties[[contextName, "external"]]
  value <- graph$contexts[[contextName]][[varName]]
  isNonce <- is.null(body(value))
  if (isNonce) {
    # I use a "function() NULL" per node as a sigil value, can just copy those
    if (written) {
      trace("   State var with nonce: `", varName, "` -> `", newName, "`\n")
    } else {
      trace("   Read-only var with nonce: `", varName, "` -> `", newName, "`\n")
    }
    dest.env[[newName]] <- value
  } else if (!is.null(key <- contains(graph$nodes, value))) {
    # the var points to one of our (old) nodes.
    # Is it written to somewhere?
    if (written) {
      trace("   State pointer: `", varName, "` -> `", newName, "`\n")
    } else {
      trace("   Static pointer?: `", varName, "` -> `", newName, "`\n")
    }
    # in either case, translate to the new function.
    trace("     with translated reference: `",
        graph$nodeContexts[[key]], "`$`", graph$nodeProperties[[key]]$localName,
        "` -> `", key, "`\n")
    dest.env[[newName]] <- dest.env[[key]]
  } else {
    # a function, but not a nonce nor recognized as one of the nodes?
    if (written) {
      trace("   State var with unknown function value(?): `",
          varName, "` -> `", newName, "`\n")
    } else {
      trace("   Read-only var with unknown function value(?): `",
          varName, "` -> `", newName, "`\n")
    }
    dest.env[[newName]] <- value
  }
}

move_value.default <- function(graph, contextName, varName, dest.env, newName,
                               varTranslations, callTranslations) {
  written <- varName %in% graph$contextProperties[[contextName, "external"]]
  trace("   State var: `", varName, "` -> `", newName, "`\n", sep="")
  dest.env[[newName]] <- graph$contexts[[contextName]][[varName]]
}

dedupe <- function(x) x[!duplicated(x)] # keeps labels
