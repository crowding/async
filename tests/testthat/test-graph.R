#' @import nseval

`%is%` <- expect_equal

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
        if (j >= sqrt(i)) {
          yield(i)
          break
        }
        j <- j + 2
      }
    }
  })

  nodeGraph <- walk(getEntry(genprimes))
  #cat(make_dot(nodeGraph),sep="\n", file = "temp.dot")
  #g <- Rgraphviz::agread("temp.dot")
  #plot(g)

})
