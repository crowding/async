# NOTHING USEFUL HERE YET

# So, let's say we have a generator/pump with a continuation

continuation <- function(obj, ...) UseMethod("continuation")

continuation.generator <- function(obj, ...) {
  environment(environment(obj$nextElem)$pump)$cont
}

continuation.async <- function(obj, ...) {
  environment(obj$state$pump)$cont
}

# things to notice as we are walking a function:
# calls into handlers?
# we should also notice when we call a callback

find_tailcalls <- function(func) {
  # find_tailcalls returns a list(list(addr=, call=), list(addr=, call=))
  make_walker(
    func$expr,
    `(`=,
    `{`=,
    `if`=,
    switch=,
    return=,
    ret=,
    stop=,
    )
  # return a list of the functions called that are called in tail position.
  # they have to actually exist and be bound...
}

make_walker <- function(...) {}

walk_tree <- function(cps) {}
