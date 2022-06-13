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

compileGraph <- function(fname, oname) {
  status <- system(
    paste("command -v dot >/dev/null 2>&1 || { echo >&2 'dot is not installed'; exit 0; } && { dot", "-Tpdf", fname, ">", oname, "; }")
  )
  expect_equal(status, 0)
}

test_that("Can extract graph of generator", {

  fname <- "temp.dot" # tempfile(fileext=".dot")
  oname <- paste0(fname, ".pdf")

  genprimes <- gen({
    yield(2)
    i <- 3
    repeat {
      j <- 3
      repeat {
        if (i/j < j) {
          yield(i)
          break
        }
        if (i %% j == 0) {
          break
        }
        j <- j + 2
      }
      i <- i + 2
    }
  })
  makeGraph(genprimes, fname)
  compileGraph(fname, oname)

  # and an async with a try-finally?
  cleanup <- FALSE
  result <- NULL
  not_run <- TRUE
  dataset <- async({
    tryCatch({
      if(FALSE) await(NULL)
      return(2)
      not_run <<- FALSE
    }, finally={
      cleanup <<- TRUE
    })
    not_run <<- FALSE
    5
  })
  makeGraph(dataset, fname)
  compileGraph(fname, oname)

  # and a generator with try/catch/finally/break/return
  fizz <- gen({
    i <- 1
    repeat {
      repeat {
        tryCatch({
          if (razz <- (i %% 2 == 0)) yield("Razz")
          if (fizz <- (i %% 3 == 0)) yield("Fizz")
          if (buzz <- (i %% 5 == 0)) yield("Buzz")
          if (razz && buzz) stop()
          if (fizz && buzz) break
          if (razz || fizz || buzz) next
          yield(toString(i))
          if (i > 30) return()
        }, error = {
          yield("\n---")
        }, finally = {
          yield("\n")
          i <- i + 1
        })
      }
      yield("<>\n")
    }
  })
  makeGraph(fizz, fname)
  compileGraph(fname, oname)

  fb <- gen({
    for (i in iseq()) {
      if (i %% 3 == 0) {
        if (i %% 5 == 0)
          yield("FizzBuzz")
        else
          yield("Fizz")
      } else {
        if (i %% 5 == 0)
          yield("Buzz")
        else
          yield(i)
      }
    }
  })
  makeGraph(fb, fname)
  compileGraph(fname, oname)


})

test_that("function inspection with all_names", {

  externConst <- 10
  externVar <- 1
  externVar2 <- 5
  f <- function(arg1, arg2) {
    arg1 <- arg1 + arg2
    temp <- arg2/arg1
    temp[2] <- arg1 * arg2
    globalVar1 <<- externVar + externConst
    externVar2[arg1] <<- temp[2]
    package::doThing(arg2, foo=temp)
    g(temp, arg1)
  }

  by_role <- by_name(all_names(f))
  by_role$arg %is% c("arg1", "arg2")
  by_role$call %is% c( "+", "/", "[<-", "*", "[", "package::doThing", "g")
  by_role$external %is% c("globalVar1", "externVar2")
  by_role$local %is% c("arg1", "temp")
  by_role$tail %is% c("g")
  by_role$var %is% c("arg1", "arg2", "temp", "externVar", "externConst",
                     "externVar2")
  unname(all_names(f, "var")) %is%
    c("arg1", "arg2", "arg2", "arg1", "temp", "arg1", "arg2", "externVar",
      "externConst", "externVar2", "arg1", "temp", "arg2", "temp",
      "temp", "arg1")

  # what needs_import
  setdiff(union(by_role$external, by_role$var),
          union(by_role$local, by_role$arg))

  locals <- sort(union(by_role$local, by_role$arg))
  locals %is% c("arg1", "arg2", "temp")
  stores <- by_role$external
  reads <- sort(setdiff(by_role$var, locals))
  reads %is% c("externConst", "externVar", "externVar2")
  # but only keep externs that aren't from packages?
  # or only externs that aren't in function heads?
})
