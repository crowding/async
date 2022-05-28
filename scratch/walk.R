library(async)

genprimes <- gen({
  yield(2)
  yield(3)
  i <- 3
  repeat {
    i <- i + 2
    j <- 3
    repeat {
      if ( i %% j == 0 ) {
        break
      }
      if (j >= sqrt(i)) {
        yield(i)
        break
      }
      j <- j + 2
    }
  }
})

graph <- walk(getEntry(genprimes))

make.graphviz <- function(graph) {
  nodes <- names(graph$nodes)
  edgeL <- structure(names=nodes, lapply(nodes, function(node) {
    edgelist <- as.list(graph$edges[[node]])
    edges = structure(match(as.character(edgelist), nodes),
                      names=ifelse(names(edgelist) == "cont", "", names(edgelist)))
    browser()
    list(edges = edges)
  }))
  gv <- new("graphNEL", nodes=nodes, edgeL=edgeL, edgemode="directed")
  browser()
  gv <- layoutGraph(gv)

  # add edge labels to some edges
  # label the nodes with short labels
  # make subgraphs that keep closures together
  # (can I draw a boundary around the subgraph?)
}
gv <- make.graphviz(walk(getEntry(genprimes)))
graph.par(list(
  nodes=list(fill="lightgray", textCol="black", lty="solid", fontsize=6),
  edges=list(col="lightblue", lty="solid")))
renderGraph(gv)




new("graphNEL",
    nodes = c("a", "b", "c", "d", "e", "f", "g",  "h", "i", "j"),
    edgeL = list(a = list(edges = c(2, 4, 5, 6, 8 )),
                 b = list(edges = c(6, 1, 4, 5, 8)),
                 c = list(edges = 8),
                 d = list(edges = c(1, 2, 5, 6, 8)),
                 e = list(edges = c(1,      2, 4, 6, 8)),
                 f = list(edges = c(2, 1, 4, 5, 8)),
                 g = list(         edges = numeric(0)),
                 h = list(edges = c(3, 1, 2, 4, 5,      6)),
                 i = list(edges = numeric(0)), j = list(edges = numeric(0))),
    edgeData = new("attrData",
                   data = list("a|b" = list(weight = 1),
                               "a|d" = list(weight = 1),
                               "a|e" = list(weight = 1),
                               "a|f" = list(             weight = 1),
                               "a|h" = list(weight = 1),
                               "b|f" = list(             weight = 2),
                               "b|a" = list(weight = 1),
                               "b|d" = list(             weight = 1),
                               "b|e" = list(weight = 1),
                               "b|h" = list(             weight = 1),
                               "c|h" = list(weight = 1),
                               "d|a" = list(             weight = 1),
                               "d|b" = list(weight = 1),
                               "d|e" = list(             weight = 1),
                               "d|f" = list(weight = 1),
                               "d|h" = list(             weight = 1),
                               "e|a" = list(weight = 1),
                               "e|b" = list(             weight = 1),
                               "e|d" = list(weight = 1),
                               "e|f" = list(             weight = 1),
                               "e|h" = list(weight = 1),
                               "f|b" = list(             weight = 2),
                               "f|a" = list(weight = 1),
                               "f|d" = list(             weight = 1),
                               "f|e" = list(weight = 1),
                               "f|h" = list(             weight = 1),
                               "h|c" = list(weight = 1),
                               "h|a" = list(             weight = 1),
                               "h|b" = list(weight = 1),
                               "h|d" = list(             weight = 1),
                               "h|e" = list(weight = 1),
                               "h|f" = list(             weight = 1)),
                   defaults = list(weight = 1)),
    nodeData = new("attrData",          data = list(), defaults = list()),
    renderInfo = new("renderInfo",
                     nodes = list(), edges = list(), graph = list(), pars = list()),
    graphData = list(edgemode = "undirected"))





# so we probably want to capture anything that the endpoint calls and
# we are closed over. that'd be "x" in this case. Hmm, but can we tell
# if we are closed over something? this is all ad hoc inspection of
# arguments, we don't have the functions any more. How about, anything
# that isn't defined in a namespace or globalenv.

# And anything that's closed over that we _call to_ is probably an endpoint.
# we take "x" and send it to....

args <- names(formals(entry))
`%-%` <- setdiff

trace <- function(f) {
  name <- as.character(substitute(f))
  force(f)
  function(...) {
    print(as.call(list(name, ...)))
    browser()
    f(...)
  }
}

all_names(body(tryCatch))
# scanning over the body to see how to collect data

#  endpoints (calls made at the tail of an if() etc.
# locals
# state
closedover <- function(env) {
  (!identical(env, globalenv())
    && !identical(env, baseenv())
    && !isNamespace(env)
    && is.null(attr(env, "name"))
    && !isBaseNamespace(env))
}
keep <- function(data, f) data[vapply(data, f, FALSE)]
closed_calls <- structure(calls, names=calls) |>
  locate_.list(environment(entry), mode="function")
closed_vars <- structure(vars, names=vars) |>
  locate_.list(environment(entry))

#todos <- by_class(all_names(body(tryCatch)))
#todos <- all.names(body(tryCatch)) |> keep(\(x)names(x))

#we should be able to traverse the graph by just pulling out tailcalls.
#we need to keep track of if we've visited ....

# I'm collecting functions, functions are identical


# now I want o see these as a graph! It would be so cool! Let's start
# figuring out R graphviz
