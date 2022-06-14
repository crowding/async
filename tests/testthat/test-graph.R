#' @import nseval

`%is%` <- expect_equal

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

})

test_that("Async with try-finally", {

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

})

test_that("try/finally/catch/break/return", {
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
})

test_that("fizzbuzz", {

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
  g1 <- function(val) NULL
  g2 <- function(val, cont, ...) NULL
  g3 <- function(val) NULL
  delayedAssign("cont", stop("don't look at me!"))
  f <- function(arg1, arg2, cont) {
    arg1 <- arg1 + arg2
    temp <- arg2/arg1
    temp[2] <- arg1 * arg2
    globalVar1 <<- externVar + externConst
    externVar2[arg1] <<- temp[2]
    package::doThing(arg2, foo=temp)
    if (FALSE) { #selection of tailcalls
      if (TRUE)
        g(temp, arg1) # a tailcall
      else
        g1(temp, arg1)
    } else {
      if(FALSE)
        g2(12, g3, NULL) # a _trampolined_ tailcall
      else
        cont(1) # tailcall into an argument, can't look it up...
    }
  }

  by_role <- by_name(all_names(f))
  by_role$arg %is% c("arg1", "arg2", "cont")
  by_role$call %is% c( "+", "/", "*", "[<-", "[", "package::doThing",
                      "g", "g1", "g2", "g3", "cont")
  by_role$external %is% c("globalVar1", "externVar2")
  by_role$local %is% c("arg1", "temp")
  by_role$tail %is% c("g", "g1", "g2", "g3", "cont")
  by_role$tramp %is% c("g3")
  by_role$var %is% c("arg1", "arg2", "temp", "externVar", "externConst",
                     "externVar2")
  unname(all_names(f, "var")) %is%
    c("arg1", "arg2", "arg2", "arg1", "arg1", "arg2", "temp", "externVar",
      "externConst", "temp", "externVar2", "arg1", "arg2", "temp",
      "temp", "arg1", "temp", "arg1")

  all_names(f, c("tailcall", "trampoline", "handler")) %is%
    list(tailcall=alist(g(temp, arg1)),
         tailcall=alist(g1(temp, arg1)),
         handler=alist(g2(val=12), g2(12, g3, NULL)),
         trampoline=alist(g3(NULL), g2(12, g3, NULL)),
         tailcall=alist(cont(1)))

  # what needs_import
  setdiff(union(by_role$external, by_role$var),
          union(by_role$local, by_role$arg)) %is% c(
            "globalVar1", "externVar2", "externVar", "externConst")

  locals <- sort(union(by_role$local, by_role$arg))
  locals %is% c("arg1", "arg2", "cont", "temp")
  stores <- by_role$external
  reads <- sort(setdiff(by_role$var, locals))
  reads %is% c("externConst", "externVar", "externVar2")
  # but only keep externs that aren't from packages?
  # or only externs that aren't in function heads?
})

test_that("all_names recognizes trampolines", {

  trample <- function(cont, ...) NULL
  y <- function(val) {
    force(val)
    trace("yield\n")
    yield(val) # these are different calls because make_async
    # wraps around make_pump and we affect state in both...
    trample(cont, val)
  }
  an <- all_names(y, c("tailcall", "trampoline", "handler"))

  an$handler %is% alist(trample(), trample(cont, val))
  an$trampoline %is% alist(cont(val), trample(cont, val))

})

test_that("all_names and args", {

  cont <- function(val) NULL
  R_ <- function() {
    trace(paste0("R: ", deparse(expr(x)), "\n"))
    set_dots(environment(), x)
    cont(...)
  }
  all_names(R_, c("external", "local", "arg", "var")) %is%
    c(var="x", var="x", var="...")

  all_names(function(x)x <<- x) %is% c(arg="x", var="x", external="x")

})
