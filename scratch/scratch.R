# For example, take the function

## x <- 0
## while(x <= 100) {x <- x + 1}
## cat(x)

# Pretend we did not have a "while" loop, or any looping construct in
# our language. How could we write a "while" loop?

## function() { if (x <= 100) {x <<- x + 1;} }

## x <- 0
## check <- function(continue) if (x <= 100) continue(TRUE) else continue(FALSE)
## doWhile <- function(checker, body, continue) {
##   doCheck <- function
##   gotCheck <- function(result) if (result) body(loop) else
## }

## doWhile(function(val, continue) continue(x <= 100),
##         function(continue) continue(x <= 100)

  if(FALSE) {

    microbenchmark(
    trans( body(tryCatch),
#      quote( f(g, x) + g(f, y) + f::g(x, quote(y)) ),
      callTranslations, varTranslations),
    transX( body(tryCatch),
 #     quote( f(g, x) + g(f, y) + f::g(x, quote(y)) ),
      callTranslations, varTranslations)
    )

  }
