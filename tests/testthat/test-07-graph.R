#' @import nseval

test_that("pasting together names", {

  paste.dropping.empty(
    c("", "x", "", "x"), c("", "", "X", "X"),
    sep=".") %is%
    c("", "x", "X", "x.X")

  paste.dropping.empty(
    c("", "x", "", "x"), c("", "", "X", "X"),
    sep=".", collapse="-") %is%
    c("x-X-x.X")

  condense.name(c("start", "cont", "cont", "test", "test",
                  "cont", "break")) %is% "start.2.test2.1.break"

})

basename <- function(name) paste0("graphs/", name, "_",
                                  as.character(compileLevel))

test_that("function inspection with all_names", {

  externConst <- 10
  externVar <- 1
  externVar2 <- 5
  if (exists("alichlkh")) rm("alichlkh", inherits=TRUE)
  g1 <- function(val) NULL
  g2 <- function(val, cont, ...) NULL
  g3 <- function(val) NULL
  g4 <- function(val) NULL
  delayedAssign("cont", stop("don't look at me!"))
  cont <- function(val, cont, ...) "wrong, don't look this up"

  f <- function(arg1, arg2, cont) {
    arg1 <- arg1 + arg2
    temp <- arg2/arg1
    temp[2] <- arg1 * arg2
    globalVar1 <<- externVar + externConst
    externVar2[arg1] <<- temp[2]
    switch("foo", a=, b=, NULL)
    package::doThing(arg2, foo=temp)
    ff <- function(val) {
      #interior lambda might update and tailcall.
      #Treatment of "local variables" for substitution
      #is tricky though, since no environments to export
      temperature <- val+arg1
      externVar <<- arg2+1
      if(FALSE) cont(12)
      else g4(temperature)
    }
    if (FALSE) { #selection of tailcalls
      if (TRUE)
        alichlkh(temp, arg1) # a tailcall to something you can't find...
      else
        g1(temp, arg1)
    } else {
      if(FALSE)
        g2(12, g3, NULL) # a _trampolined_ tailcall
      else
        cont(1) # "cont" is an argument, not the trampoline above
    }
  }

  by_role <- by_name(all_names(f))
  by_role$arg %is% c("arg1", "arg2", "cont")
  by_role$call %is% c( "+", "/", "*", "[<-", "[", "package::doThing",
                      "cont", "g4", "alichlkh", "g1", "g2", "g3")
  by_role$store %is% c("globalVar1", "externVar2", "externVar")
  by_role$local %is% c("arg1", "temp", "temperature",
                       "ff") #local in the local function I guess...
  by_role$tail %is% c("cont", "g4", "alichlkh", "g1", "g2", "g3")
  by_role$tramp %is% c("g3")
  by_role$var %is% c("arg1", "arg2", "temp", "externVar", "externConst",
                     "externVar2", "temperature")
  unname(all_names(f, "var")) %is%
    c("arg1", "arg2", "arg2", "arg1", "arg1", "arg2", "temp",
      "externVar", "externConst", "temp", "externVar2", "arg1", "arg2",
      "temp", "arg1", "arg2", "temperature", "temp", "arg1", "temp", "arg1")

  all_names(f, c("tailcall", "trampoline", "handler")) %is%
    list(tailcall=alist(alichlkh(temp, arg1)),
         tailcall=alist(g1(temp, arg1)),
         handler=alist(g2(val=12), g2(12, g3, NULL)),
         trampoline=alist(g3(NULL), g2(12, g3, NULL)),
         tailcall=alist(cont(1)))

  rm("cont")

  # what needs_import
  setdiff(union(by_role$store, by_role$var),
          union(by_role$local, by_role$arg)) %is% c(
            "globalVar1", "externVar2", "externVar", "externConst")

  locals <- sort(union(by_role$local, by_role$arg))
  locals %is% c("arg1", "arg2", "cont", "ff", "temp", "temperature")
  stores <- by_role$store
  reads <- sort(setdiff(by_role$var, locals))
  reads %is% c("externConst", "externVar", "externVar2")
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

  z <- function(err) {
    trace(paste0("pump: stop: ", conditionMessage(err), "\n"))
    err <<- err
    action <<- "stop"
    stop(err)
  }
  #stop is not a trampoline call bc no "cont"
  an <- all_names(z, c("tailcall", "trampoline", "handler"))

  pause <- function(cont, ...) NULL
  w <- function(val, cont, ...) {
    trace("generator: yield\n")
    yielded <<- val
    state <<- "yielded"
    pause(cont, ...)
  }
  #"cont" is not registered as a trampoline because it's indirect
  #(i.e. cont is in the args)
  all_names(w, c("tailcall", "trampoline", "handler"))

})

test_that("all_names and args", {

  cont <- function(val) NULL
  R_ <- function() {
    trace(paste0("R: ", deparse(expr(x)), "\n"))
    set_dots(environment(), x)
    cont(...)
  }
  all_names(R_, c("store", "local", "arg", "var")) %is%
    c(var="x", var="x", var="...")
  all_names(function(x)x <<- x) %is% c(arg="x", var="x", store="x")

})

test_that("Can extract graph of generator", {

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

  expect_silent(drawGraph(genprimes, basename("genprimes")))
})

test_that("tryCatch", {

  # hmm. Where is yield("!")? the graph traversal didn't get it.
  seq <- ilimit(iseq(), 50)
  fizztry <- gen({
    tryCatch({
      repeat {
        i <- nextElem(seq)
        if(i %% 3 == 0) {
          if (i %% 5 == 0) {
            yield("FizzBuzz")
          } else {
            yield("Fizz")
          }
        } else if (i %% 5 == 0) {
          yield("Buzz")
        }
      }
    }, error=function(e) NULL)
    yield("!")
  })
  expect_silent(drawGraph(fizztry, basename("fizztry")))

})

test_that("nextElemOr", {

  x <- iseq(1, 55)
  incomplete <- gen(split_pipes=TRUE, {
    repeat {
      sum <- 0
      for (i in 1:10) {
        sum <- nextElemOr(x, {yield(sum); return()}) + sum
      }
      yield(sum)
    }
  })

  expect_silent(drawGraph(incomplete, basename("incomplete")))
})

test_that("collatz", {

  collatz <- function(x) {
    x <- as.integer(x)
    gen(trace=cat, {
      yield(x)
      repeat {
        if (x %% 2L == 0) {
          x <- yield(x %/% 2L)
        } else {
          x <- yield(3L * x + 1L)
        }
      }
    })
  }
  collatz11 <- collatz(11L)
  expect_silent(drawGraph(collatz11, basename("collatz11")))

})

test_that("yieldFrom", {

  gchain <- function(its) { force(its)
    gen(for (it in its) yieldFrom(it))
  }
  achain <- gchain(list(c("a", "b", "c"), c(1, 2, 3)))
  expect_silent(drawGraph(achain, basename("achain")))

  achain2 <- gen(for (it in its) for (i in it) yield(i))
  expect_silent(drawGraph(achain2, basename("achain2")))

})

test_that("Async with try-finally", {

  cleanup <- FALSE
  result <- NULL
  not_run <- TRUE
  tryfin <- async({
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

  ## wait_for_it()
  ## expect_true(not_run)
  ## expect_false(cleanup)
  expect_silent(drawGraph(tryfin, basename("tryfin"),
                          vars=TRUE, envs=TRUE, handlers=TRUE))

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
  expect_silent(drawGraph(fizz, basename("fizz"),
                          vars=FALSE, envs=FALSE, handlers=FALSE))

})

test_that("fizzbuzz", {

  fizzbuzz <- gen({
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
  expect_silent(drawGraph(fizzbuzz, basename("fizzbuzz"),
                          vars=TRUE, envs=FALSE))

  nicebuzz <- gen({
    tryCatch(
      for (i in iseq()) {
        if (i %% 69 == 0) {
          if (i %% 420 == 0)
            stop("Whoa!")
          else
            yield("nice")
        } else {
          if (i %% 420 == 0) {
            yield("Nice")
            break
          } else
            yield(i)
        }
      },
      finally=yield("Goodnight")
    )
  })
  expect_silent(drawGraph(nicebuzz, basename("nicebuzz"),
                          handlers=TRUE, vars=TRUE, envs=FALSE))

})
