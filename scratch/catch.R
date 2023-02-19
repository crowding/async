
#' @title Named, nonlocal exits.
#'
#' `catch(name, expr) evaluates and returns `expr`. If during the
#' evaluation of `expr` `throw(name, ...)` is called with the same
#' name, evaluation of `expr` terminates and the argument to `throw`
#' is returned instead.
#'
#' `throw` and `catch` can be seen as a generalized version of
#' `return`. While `return` stops executing the enclosing function and
#' returns from its enclosing function with the given value,
#' `throw(name, value)` will stop execution and return from the
#' nearest enclosing `catch` with the same name. This can be used as a
#' "local return" where `catch` and `throw` are the same function; it
#' can also be used as a "nonlocal return" where the matching `catch`
#' and `throw` are separated by several function calls.
#'
#' Despite similar naming, `catch` and `throw` do not use the
#' [conditions] system and will not catch errors from `stop`
#' -- `catch` only catches uses of `throw`.
#'
#' `catch` and `throw` are patterned after the forms of the same
#' name in Common Lisp.
#'
#' @examples
#'
#' @param name A character string to name the catch. If multiple
#'   `catch` having the same name are active, then `throw` will return
#'   from the innermost.
#' @param expr Any expression to be evaluated.
#' @param handler Optional; defaults to [`identity`]. This function will
#'   be applied to any thrown values. (If you specify a handler that
#'   takes multiple arguments, you can give multiple arguments to
#'   throw() and they will all be passed along.)
#' @return The value of `expr`, unless `throw(name, ...)` was called
#'   while forcing `expr`, in which case the value of `handler(...)`.
#' @author Peter Meilstrup
#'
#' @seealso on.exit
catch <- function(name, expr,
                  handler = function(x=invisible(NULL)) x) {
  result <- NULL
  if (exists(name, envir=catch_stack, inherits=FALSE)) {
    on.exit(catch_stack <<- parent.env(catch_stack))
    catch_stack <<- new.env(parent=catch_stack)
  } else {
    on.exit(rm(name, envir=catch_stack))
  }
  # the trick is putting a `break` in a lazy argument `trip`.
  catch_stack[[name]] <-
    (function(trip) function(...) {result <<- fn(...); trip})(break)
  repeat {
    # if `trip` is forced, the `break` will unwind the stack back to
    # the environment it was promised in, exiting from this repeat.
    result <- expr
    break # exit from this repeat anyway
  }
  result
}
catch_stack <- emptyenv()

#' @rdname catch
#' @param ... Arguments given to `throw` will be passed along to
#' the corresponding `handler`.
throw <- function(name, ...) {
  mget(name, catch_stack,
       ifnotfound=stop("There is no active catch() by that name")
  )[[1]](...)
}


if (FALSE) {
  ## `throw` can be used to break out of nested loop
  ## (where `break` only stops one...)
  found <- catch("found", {
    for (row in 1:height) {
      for (col in 1:width) {
        if (isItAt(row, col)) throw("found", TRUE)
      }
    }
    FALSE
  })
  if(found) doStuffAt(row, col)

  ## "throw" can also be used to return from a nested, recursive search

}


if(FALSE) {"can you always break from down the stack?"
  # really, what about if you're in the middle of a C call?
  thunk3 <- function(a, b) {
    on.exit(print("exit3"))
    do(thunk2, dots(a, b))
  }
  thunk2 <- function(a, b) {
    on.exit(print("exit2"))
    do.call("thunk", alist(a, b))
  }
  thunk <- function(a, b) {
    on.exit(print("exit"))
    if(a) b
  }
  i <- 0
  repeat {
    print(i <- i + 1)
    thunk3(i>5, break)
  }
}
