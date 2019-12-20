reset <- function(...) list(...)[[1]] #evaluates all args then returns first.

add_class <- function(x, ...) {
  attr(x, "class") <- c(..., attr(x, "class"))
  x
}

pump <- function(expr) make_pump(expr)()

make_pump <- function(expr, ...,
                      ret=base::stop("unused"),
                      stop=base::stop("unused")) {
  nonce <- function() NULL
  cont <- expr
  value <- nonce

  on_finish <- function(...) {
    log("pump finishing with value")
    value
  }

  ret <- function(cont, ...) {
    log(where <- "pump ret")
    list(cont, ...) #force
    #log(deparse(list(...)))
    log(where <- "pump ret forced")
    cont <<- function() {
      log(where <- "Thunk called")
      #log(deparse(list(cont, ...)))
      cont(...)
    }
  }

  ## ret <- function(cont, ...) {
  ##   log(where <- "pump fake ret")
  ##   list(cont, ...) #force
  ##   cont(...)
  ## }

  stop <- function(...) {
    log(where <- "pump stop")
    log(deparse(list(...)))
    on_finish <<- function() {
      log("pump finishing with error")
      #log(deparse(list(...)))
      base::stop(...)
    }
  }

  got_value <- function(val) {
    log(where <- "pump got value")
    value <<- val
  }

  function(new_cont) {
    if (missing(new_cont)) {
      result <- cont(got_value, ..., ret=ret, stop=stop)
    } else {
      # the continuation given to us here should be one
      # that the yield/block handler exfiltrated,
      # so context should already be established
      result <- new_cont()
    }
    log(where <- "pump first return")
    while(!identical(cont, nonce)) {
      log(where <- "pump thunk")
      (reset(cont, cont <<- nonce))()
    }
    log(where <- "pump finish")
    cont <<- on_finish
    on_finish()
  }
}



make_generator <- function(expr, ...) {
  nonce <- function() NULL
  cont <- nonce
  yielded <- nonce

  yield <- function(cont, val) {
    log("Yield handler called")
    cont <<- function() cont(val) # yield() returns its input
    yielded <<- val
    val
  }

  pump <- make_pump(expr, ..., yield=yield)

  nextElem <- function(...) {
    log("nextElem")
    result <- tryCatch(
      if (identical(cont, nonce)) pump() else pump(reset(cont, cont <<- nonce)),
      error = function(e) {
        log("nextElem pump threw error")
        if (identical(conditionMessage(e), 'StopIteration'))
          e else stop(e)
      })
    if (identical(cont, nonce)) {
      log("nextElem reached end")
      cont <<- function() stop("StopIteration")
      if (identical(yielded, nonce)) {
        stop("StopIteration")
      } else {
        log("nextElem returning value yielded from end")
        reset(yielded, yielded <<- nonce)
      }
    } else if (identical(yielded, nonce)) {
      warning("nextElem: neither yielded nor finished")
    } else {
      log("nextElem returning yielded value")
      reset(yielded, yielded <<- nonce)
    }
  }

  add_class(itertools::new_iterator(nextElem), "generator")
}


make_delay <- function(expr, ...) {
  nonce <- function() NULL
  cont <- nonce

  block <- function(cont) {
    cont <<- cont
  }

  pump <- make_pump(expr, ..., block=block)

  check <- function(resolve, reject) {
    result <- tryCatch({
      if (identical(cont, nonce)) {
        #first time
        pump()
      } else {
        #subsequent times
        pump(reset(cont, cont <<- nonce))
      }
      if (identical(cont, nonce)) {
        #last time
        resolve(result)
      }},
      error = function(e) reject(e))
  }

  add_class(promises::promise(check), "delay")
}
