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

environment(genprimes$nextElem)
ls(environment(genprimes$nextElem)) # make_generator closure
environment(environment(genprimes$nextElem)$pump) # make_pump closure
ls(environment(environment(genprimes$nextElem)$pump)) # here's all the pump state
entry <- environment(environment(genprimes$nextElem)$pump)$cont # the entry point, it's an R()

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
# Scan an expression and return all names used, along with their "roles":
# var, call, local, state, tailcall
all_names <- function(expr,
                      types=c("call", "var", "local", "state", "tail"),
                      call="call" %in% types,
                      var="var" %in% types,
                      local="var" %in% types,
                      state="state" %in% types,
                      tail="tail" %in% types) {
  concat <- function(l) do.call("c", l)
  collect_head <- function(expr, tail)
    switch(mode(expr),
           call= collect_call(expr, tail=FALSE),
           character=if(call) c(call=expr),
           name=c(if(call) c(call=as.character(expr)),
                  if(tail) c(tail=as.character(expr))),
           character(0))
  collect_call <- function(expr, tail=TRUE)
    if (length(expr) > 1)
      switch(mode(expr[[1]]),
             character=,
             name=switch(
               as.character(expr[[1]]),
               "=" =, "<-" = if (local) c(local=collect_store(expr[[2]]),
                                        collect_arg(expr[[3]], tail=FALSE)),
               "<<-" = if (state) c(state=collect_store(expr[[2]]),
                                    collect_arg(expr[[3]], tail=FALSE)),
               "if" = c(collect_arg(expr[[2]], tail=FALSE),
                        collect_arg(expr[[3]], tail=tail),
                        if (length(expr) >= 4) collect_arg(expr[[4]], tail=tail)),
               "ret" =,
               "return" = collect_arg(expr[[2]], tail=TRUE),
               "while" = c(collect_arg(expr[[2]], tail=FALSE),
                           collect_arg(expr[[3]], tail=tail)),
               "for" = c(collect_arg(expr[[2]], tail=FALSE),
                         collect_arg(expr[[3]], tail=FALSE),
                         collect_arg(expr[[4]], tail=tail)),
               "("= collect_arg(expr[[1]], tail=tail),
               "{"= c(concat(lapply(expr[c(-1,-length(expr))],
                                    collect_arg, tail=FALSE)),
                      collect_arg(expr[[length(expr)]], tail=tail)),
               "||"=,"&&"=c(collect_arg(expr[[2]], tail=FALSE),
                            collect_arg(expr[[3]], tail=tail)),
               "switch"=c(collect_arg(expr[[2]], tail=FALSE),
                          concat(lapply(expr[-1], collect_arg, tail=tail))),
               "function"=character(0),
               # general function calls
               c(collect_head(expr[[1]], tail=tail),
                 concat(lapply(expr[-1], collect_arg, tail=FALSE)))
             ),
             # a call with a call in its head?
             c(collect_head(expr[[1]], FALSE),
               concat(lapply(expr[-1], collect_arg, tail=tail)))
             ) else collect_head(expr[[1]], tail=tail)
  collect_arg <- function(expr, tail)
    switch(mode(expr),
           call=collect_call(expr, tail),
           name=if(var) c(var=as.character(expr)) else character(0),
           character(0))
  collect_store <- function(dest)
    switch(mode(dest),
           call=collect_store(dest[[2]]),
           character=dest,
           name=as.character(dest),
           character(0))
  ## collect_call <- trace(collect_call)
  ## collect_store <- trace(collect_store)
  ## collect_arg <- trace(collect_arg)
  ## collect_head <- trace(collect_head)
  collect_arg(expr, tail=tail)
}




all_names(body(tryCatch))
# scanning over the body to see how to collect data

#endpoints (calls made at the tail of an if() etc.
#locals
#state
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
unname <- function(x) `names<-`(x, NULL)
by_class <- function(vec) {
  out <- list()
  for (class in unique(names(vec))) {
    out[[class]] <- unname(vec[names(vec) == class])
  }
  out
}

todos <- by_class(all_names(body(tryCatch)))

todos <- all.names(body(tryCatch)) |> keep(\(x)names(x))


#we should be able to traverse the graph by just pulling out tailcalls.
#we need to keep track of if we've visited ....

# I'm collecting functions, functions are identical

contains <- function(env, candidate, cmp=identical) {
  # this should be a hashset or something...
  for (key in names(env))
    if (cmp(candidate, env[[key]]))
      return(key)
  return(NULL)
}

# collect all nodes and name them
# collect all the transitions and name them...

walk <- function(start) {
  iter <- icount()
  namer <- function(x) paste0("c", nextElem(iter))
  nodes <- new.env()
  edges <- new.env()
  doWalk <- function(entry) {
    if (!is.null(this <- contains(nodes, x)))
      return(this)
    nodes[[this <- namer(entry)]] <- entry
    edges[[this]] <- new.env()
    names <- by_class(all_names(body(entry)))
    for (that in names$tail) {
      exit <- get(that, envir=environment(entry))
      thatName <- doWalk(exit)
      cat(sprintf("%s:%s -> %s\n", thatName))
      edges[[this]][[that]] <- thatName
    }
  }
  doWalk(start)
  list(nodes=nodes, edges=edges)
}

graph <- walk(entry)

for (i in names(graph$nodes)
#collect all nodes and the names of the links...
