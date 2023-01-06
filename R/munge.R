munge <- function(# the async/generator to munge
                  g,
                  # What is the destination env?  In the interpreted
                  # form the effective parent environment is captured
                  # in the "R_" objects.  In the compiled form it might
                  # be the a child of the base environment that called
                  # gen() (different for each invocation!) So we don't
                  # know yet, so just munge to a new env and set its
                  # parent later.
                  dest.env = new.env(parent = baseenv())) {
  # The graph data structure should give us most info we need.
  graph <- walk(g)
  dest.env$.contextName <- "."

  # Collect information in the storage used by the functions in each context.
  for (contextName in names(graph$contexts)) {
    trace_(paste0("Context: ", contextName, "\n"))
    context <- graph$contexts[[contextName]]
    props <- graph$contextProperties[[contextName]]
    contextVars <- unique(c(props$read, props$store))
    # Make up translated names of variables and nodes
    varTranslations <- structure(
      as.character(paste0(contextName, "_v_", contextVars, recycle0=TRUE)),
      names=contextVars)

    calls <- unlist(as.list(props, all.names=TRUE)[c("tail", "tramp", "hand", "windup", "utility")],
                    use.names=FALSE)

    # The local labels for each edge are collected in edges.
    callTranslations <- (
      names(graph$contextNodes[[contextName]])
      |> lapply(
        \(nodeName) (
          graph$nodeEdgeProperties[[nodeName]]
          |> as.list(all.names=TRUE)
          |> vapply(\(x) x$to, "")))
      |> concat())

    utils <- setdiff(setdiff(props$utility,
                             names(callTranslations)),
                     contextVars)
    utilTranslations <- structure(
      paste0(contextName, "_f_", utils, recycle0=TRUE),
      names=utils)

    # move_value may need to translate a state pointer, and so needs
    # to have the nodes already moved. So moving vars happens after
    # nodes. On the other hand, if in the future we want to dedupe
    # constants while moving, we should move values _before_ nodes, so
    # that we can move nodes with a better translation table.  (so
    # possibly move/dedupe constants first, then state vars later.)
    if (length(varTranslations) > 0) {
      trace_(" Moving constants:\n")
      f <- is_forced_(names(varTranslations), context)
      if (any(!f)) {
        stop("Unforced arguments found in munging: ",
             paste(names(f)[[!f]], collapse=", "))
      }
      for (varName in setdiff(names(varTranslations),
                              graph$contextProperties[[contextName]]$store)) {
        newName <- varTranslations[[varName]]
        move_value(graph, contextName, varName, dest.env, newName,
                   varTranslations, callTranslations)
      }
    }
    trace_(" Moving nodes:\n")
    # each name should have a clearly defined role.
    # don't pass functions as arguments outside of a trampoline call
    if (length(which <- intersect(names(callTranslations),
                                  names(varTranslations))) > 0) {
      stop("Name `", which, "` appears as both a tailcall and variable")
    }
    if (length(which <- intersect(names(callTranslations),
                                  names(utilTranslations))) > 0) {
      stop("Name `", which, "` appears as both a tailcall and ordinary call")
    }
    if (length(which <- intersect(names(varTranslations),
                                  names(utilTranslations))) > 0) {
      stop("Name `", which, "` appears as both a variable and call")
    }
    nms <- c(varTranslations, callTranslations, utilTranslations)

    for (nodeName in names(graph$contextNodes[[contextName]])) {
      # nodeName is the translated node name that walk() came up with
      node <- graph$nodes[[nodeName]]
      nodeBody <- body(node)
      locals <- names(formals(node))
      gnms <- nms[!(names(nms) %in% locals)]
      transBody <- trans(nodeBody, gnms, gnms)
      trace_(paste0("   Node: `", contextName, "`$`",
                    graph$nodeProperties[[nodeName]]$localName,
                    "` -> `", nodeName, "`\n"))
      dest.env[[nodeName]] <-
        structure(function_(formals(node), transBody, dest.env),
                  localName=nodeName, globalName=nodeName)
    }
    if (length(utilTranslations) > 0) {
      trace_(" Moving utils:\n")
      for (fnam in names(utilTranslations)) {
        func <- graph$contexts[[contextName]][[fnam]]
        if (identical(environment(func), context)) {
          trace_(paste0("   Companion function: `", contextName, "`$`", fnam,
                        "` -> `", utilTranslations[[fnam]], "`\n"))
          funcBody <- body(node)
          transBody <- trans(funcBody, nms, nms)
          dest.env[[nodeName]] <-
            function_(formals(node), transBody, dest.env)
        } else {
          newName <- utilTranslations[[fnam]]
          trace_(paste0("   External function: `", contextName, "`$`", fnam,
                        "` -> `", newName, "`\n"))
          move_value(graph, contextName, fnam, dest.env, newName,
                     varTranslations, callTranslations)
        }
      }
    }
    if (length(varTranslations) > 0) {
      trace_(" Moving state:\n")
      for (varName in intersect(graph$contextProperties[[contextName]]$store,
                                names(varTranslations))) {
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
  value <- get(varName, graph$contexts[[contextName]])
  UseMethod("move_value", value)
}

move_value.quotation <- function(graph, contextName, varName, dest.env, newName,
                                 varTranslations, callTranslations) {
  # quotations can be of mode "function," but should be moved directly
  # _without_ modifying their environment.
  dest.env[[newName]] <-
    graph$contexts[[contextName]][[varName]]
}

move_value.function <- function(graph, contextName, varName, dest.env, newName,
                                varTranslations, callTranslations) {
  written <- varName %in% graph$contextProperties[[contextName, "store"]]
  value <- get(varName, graph$contexts[[contextName]])
  isNonce <- is.null(body(value))
  if (isNonce) {
    # I use a "function() NULL" per node as a sigil value, can just copy those
    if (written) {
      trace_(paste0("   State var with nonce: `",
                    varName, "` -> `", newName, "`\n"))
    } else {
      trace_(paste0("   Read-only var with nonce: `",
                    varName, "` -> `", newName, "`\n"))
    }
    dest.env[[newName]] <- value
  } else if (!is.na(key <- contains(graph$nodes, value))) {
    # the var points to one of our (old) nodes.
    # Is it written to somewhere?
    if (written) {
      trace_(paste0("   State pointer: `",
                    varName, "` -> `", newName, "`\n"))
    } else {
      trace_(paste0("   Static pointer?: `",
                    varName, "` -> `", newName, "`\n"))
    }
    # in either case, update the pointer.
    trace_(paste0("     with translated reference: `",
                  graph$nodeContexts[[key]],
                  "`$`", graph$nodeProperties[[key]]$localName,
                  "` -> `", key, "`\n"))
    ##dest.env[[newName]] <- dest.env[[key]]
    # Note that since we haven't moved all the nodes from all the
    # contexts yet we will make this assignment lazy; by the time we get to
    # _run_, we will have filled in the node.
    set_arg_(quo_(newName, dest.env), quo_(as.name(key), dest.env))
  } else {
    # a function, but not a nonce nor recognized as one of the nodes?
    if (written) {
      if (varName %in% graph$contextProperties[[contextName, "tail"]])
        stop("what")
      trace_(paste0("   State var with unknown function value(?): `",
                    varName, "` -> `", newName, "`\n"))
    } else {
      trace_(paste0("   Read-only var with unknown function value(?): `",
                    varName, "` -> `", newName, "`\n"))
    }
    dest.env[[newName]] <- value
  }
}

move_value.default <- function(graph, contextName, varName, dest.env, newName,
                               varTranslations, callTranslations) {
  written <- varName %in% graph$contextProperties[[contextName, "external"]]
  if (written) {
    trace_(paste0("   State var: `", varName, "` -> `", newName, "`\n"))
  } else {
    trace_(paste0("   Constant: `", varName, "` -> `", newName, "`\n"))
  }
  dest.env[[newName]] <- get(varName, graph$contexts[[contextName]])
}

dedupe <- function(x) x[!duplicated(x)] # keeps labels
