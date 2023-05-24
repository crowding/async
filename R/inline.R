# Take a munged graph and do partial evaluation / function substitution.

inline <- function(graph) {
  inlined <- new.env()
  for (nodeName in names(graph$nodes)) {
    fn <- inline_node(graph$nodes[[nodeName]], graph, nodeName)
    graph$nodes[[fn]] <- node
  }
}

inline_node <- function(fn, graph, nodeName) {
  body(fn) <- inline_expr(body(fn), graph, nodeName, inTail=TRUE)
  fn
}

inline_expr <- function(expr, graph, nodeName, inTail) {
  switch(typeof(expr),
         language = inline_call(expr, graph, nodeName, inTail),
         symbol = {
           varName <- as.character(expr)
           cxtp <- graph$contextProperties[[graph$nodeContexts[[nodeName]]]]
           if (varName %in% cxtp$read
               && !(varName %in% cxtp$store)) {
             val <- environment(graph$nodes[[nodeName]])[[varName]]
             if (is.language(val)) {
               call("quote", val)
             } else val
           } else expr
         },
         expr)
}

inline_call <- function(expr, graph, nodeName, inTail) {
  head <- expr[[1]]
  switch(
    typeof(head),
    symbol = switch(
      as.character(head),
      `{` = inline_brace(expr, graph, nodeName, inTail),
      `if` = inline_if(expr, graph, nodeName, inTail),
      `!` = inline_not(expr, graph, nodeName, inTail),
      `||` = inline_or(expr, graph, nodeName, inTail),
      `&&` = inline_and(expr, graph, nodeName, inTail),
      `==` = inline_equals(expr, graph, nodeName, inTail),
      `!=` = inline_ne(expr, graph, nodeName, inTail),
      { # inline a regular tailcall
        callName <- as.character(head)
        if (inTail) {
          if (exists(callName,
                     graph$nodeEdgeProperties[[nodeName]])) {
            targetNode <- graph$nodes[[
              graph$nodeEdgeProperties[[nodeName]][[callName]]$to]]
            switch(
              graph$nodeEdgeProperties[[nodeName]][[callName]]$type,
              tramp =,
              tail = {
                #inline tailcalls if they have only one incoming edge
                if (length(graph$reverseEdges[[targetNode]] <= 1)) {
                  inline_tailcall(expr, graph, targetNode)
                }
              },
              hand = {
                #always inline handlers
                args <- as.list(match.call(graph$nodes[[callName]], expr))[-1]
                expr <- substituteDirect(body(graph$nodes[[callName]]), args)
                inline_expr(expr, graph, callName, inTail)
              },
              stop("Unexpected edge type")
              )
          } else inline_args(expr, graph, nodeName)
        } else inline_args(expr, graph, nodeName)
      }),
    { #non-symbol in head?
      expr[[1]] <- inline_expr(expr[[1]], graph, nodeName, inTail)
      inline_args(expr, graph, nodeName)
    }
  )
}

inline_tailcall <- function(expr, graph, targetNode, inTail) {
  #XXX: this duplicates work, Should rather update a tailcalled node
  #before substituting.
  args <- match.call(graph$nodes[[targetNode]], expr)
  expr <- substitute(body(nodes[[targetNode]]), args)
  inline_expr(expr, graph, targetNode, inTail)
}

inline_args <- function(expr, graph, nodeName) {
  expr <- as.list(expr)
  for (i in seq(2, length(expr), by=1)) {
    expr[[i]] <- inline_expr(expr[[i]], graph, nodeName,
                             inTail = FALSE)
  }
  as.call(expr)
}

inline_brace <- function(expr, graph, nodeName, inTail) {
  expr <- as.list(expr)

  if (length(expr) > 2) {
    for (i in seq(2, length(expr) - 1, by = 1)) {
      expr[[i]] <- inline_expr(expr[[i]], graph, nodeName,
                               inTail = FALSE)
    }
    expr[[length(expr)]] <- inline_expr(expr[[length(expr)]], graph, nodeName,
                                        inTail = inTail)
  } else if (length(expr) == 2) {
    expr <- inline_expr(expr[[2]],
                  graph, nodeName, inTail = inTail)
  }
  as.call(expr)
}

inline_if <- function(expr, graph, nodeName, inTail) {
  expr <- as.list(expr)
  expr[[2]] <- inline_expr(expr[[2]], graph, nodeName, FALSE)
  if (isTRUE(expr[[2]])) {
    inline_expr(expr[[3]], graph, nodeName, inTail)
  } else if (isFALSE(expr[[2]])) {
    if (length(expr) >= 4)
      inline_expr(expr[[4]], graph, nodeName, inTail)
    else quote({})
  } else {
    expr[[3]] <- inline_expr(expr[[3]], graph, nodeName, inTail)
    if (length(expr) >= 4)
      expr[[4]] <- inline_expr(expr[[4]], graph, nodeName, inTail)
    as.call(expr)
  }
}

inline_not <- function(expr, graph, nodeName, inTail) {
  expr <- as.list(expr)
  expr[[2]] <- inline_expr(expr[[2]], graph, nodeName, FALSE)
  if (isTRUE(expr[[2]])) {
    quote(FALSE)
  } else if (isFALSE(expr[[2]])) {
    quote(TRUE)
  } else {
    as.call(expr)
  }
}

inline_or <- function(expr, graph, nodeName, inTail) {
  expr <- as.list(expr)
  expr[[2]] <- inline_expr(expr[[2]], graph, nodeName, FALSE)
  expr[[3]] <- inline_expr(expr[[3]], graph, nodeName, FALSE)
  if (isTRUE(expr[[2]])) {
    quote(TRUE)
  } else if (isFALSE(expr[[2]])) {
    expr[[3]]
  } else {
    as.call(expr)
  }
}

inline_and <- function(expr, graph, nodeName, inTail) {
  expr <- as.list(expr)
  expr[[2]] <- inline_expr(expr[[2]], graph, nodeName, FALSE)
  expr[[3]] <- inline_expr(expr[[3]], graph, nodeName, FALSE)
  if (isFALSE(expr[[2]])) {
    quote(FALSE)
  } else if (isTRUE(expr[[2]])) {
    expr[[3]]
  } else {
    as.call(expr)
  }
}

inline_equals <- function(expr, graph, nodeName, inTail) {
  expr <- as.list(expr)
  expr[[2]] <- inline_expr(expr[[2]], graph, nodeName, FALSE)
  expr[[3]] <- inline_expr(expr[[3]], graph, nodeName, FALSE)
  if (is.language(expr[[2]]) || is.language(expr[[3]])) {
    as.call(expr)
  } else {
    expr[[2]] == expr[[3]]
  }
}

inline_ne <- function(expr, graph, nodeName, inTail) {
  expr <- as.list(expr)
  expr[[2]] <- inline_expr(expr[[2]], graph, nodeName, FALSE)
  expr[[3]] <- inline_expr(expr[[3]], graph, nodeName, FALSE)
  if (is.language(expr[[2]]) || is.language(expr[[3]])) {
    as.call(expr)
  } else {
    expr[[2]] != expr[[3]]
  }
}
