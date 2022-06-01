#' @import nseval

`%is%` <- expect_equal

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



test_that("Can extract graph of generator", {

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
        if (i/j < j) {
          yield(i)
          break
        }
        j <- j + 2
      }
    }
  })
  # can gather and write a graph (and render it, if dot is installed.)
  fname <- "temp.dot" # tempfile(fileext=".dot")
  makeGraph(genprimes, fname)
  oname <- paste0(fname, ".pdf")
  status <- system(
    paste("command -v dot >/dev/null 2>&1 || { echo >&2 'dot is not installed'; exit 0; } && { dot", "-Tpdf", fname, ">", oname, "; }")
  )
  expect_equal(status, 0)

  # and an async with a try-finally
  
})

