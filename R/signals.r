# arg_cps interfaces between normal R evluation and the CPS system.
#
# It saves its unevaluated arg and feeds it to the continuation function.
arg_cps <- function(x) {
  x <- arg(x)
  function(cont,
           ...,
           ret = function(cont, ...) cont(...),
           stop = base::stop,
           wind) {
    trace(where <- "arg_cps inner")
    ## If we have been handed a "wind" function, use that
    if (missing(wind)) {
      do(cont, x)
    } else {
      wind(function() do(cont, x))
    }
  }
}

try_cps <- function(expr) {
  force(expr)
  windup <- function(cont, ...) {
    trace(where <- "winding up")
    try(cont(...))
  }

  function(cont, ret, ..., wind) {
    trace(where <- "try_cps inner")
    if(missing(wind)) {
      # add a "wind" function onto the continuation arguments.
      trace(where <- "try_cps adding wind")
      expr(cont, ret=ret, ..., wind=windup)
    } else {
      # wrap the existing "wind" function
      trace(where <- "try_cps wrapping wind")
      expr(cont, ret=ret, ..., wind=function(cont, ...) wind(windup, cont, ...))
    }
  }
}



## on.exit needs to wind up a tryCatch...

tryCatch_cps <- function(expr, ..., finally) {
  list(expr, finally)
  handlers <- list(...)
  function(cont, ret, ...) {
    
  }
}
