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

if (!dir.exists("graphs")) dir.create("graphs")

filename <- function(name) paste0("graphs/", name, "_",
                                  as.character(getOption("async.compileLevel")))

test_that("function inspection with all_names", {

  externConst <- 10
  externVar <- 1
  externVar2 <- 5
  if (exists("alichlkh")) rm("alichlkh", inherits=TRUE)
  g1 <- function(val) NULL
  g2 <- function(cont, val) NULL
  g3 <- function() NULL
  g4 <- function(val) NULL
  g5 <- function(winding, cont) NULL
  wu <- function(cont) NULL
  ute <- function(x) NULL

  delayedAssign("cont", stop("don't look at me!"))
  cont <- function(val, cont, ...) "wrong, don't look this up"

  f <- function(arg1, arg2, cont) {
    arg1 <- arg1 + arg2
    temp <- arg2/arg1
    temp[2] <- arg1 * arg2
    externVar <<- + externConst + unknownConst
    externVar2[arg1] <<- temp[2]
    switch("foo", a=, b=, NULL)
    package::doThing(arg2, foo=temp)
    ff <- function(val) {
      #interior lambda might update and tailcall.
      #Treatment of "local variables" for substitution
      #is tricky though, since no environments to export
      temperature <- val+arg1
      for (i in 1) externVar2[arg1] <<- ute(arg2+i)
      if(FALSE) cont(i)
      else g4(temperature)
    }
    for (j in 1:10) NULL
    if (FALSE) { #selection of tailcalls
      on.exit({g3()})
      switch("hello",
        alichlkh(temp, arg1), # a tailcall to something you can't find...
        g1(temp),
        g5(wu, g3)
        )
    } else {
      if(FALSE)
        g2(g4, 12) # a _trampolined_ tailcall
      else
        cont(1) # "cont" is an argument, not the trampoline above
    }
  }

  nms <- all_names(f)
  by_role <- by_name(nms)
  by_role$arg %is% c("arg1", "arg2", "cont")
  by_role$call %is% c( "+", "/", "*", "[<-", "[", "package::doThing",
                      ":", "alichlkh")
  by_role$store %is% c("externVar", "externVar2")
  by_role$read %is% c("externConst", "externVar2")
  by_role$local %is% c("arg1", "temp", "ff", "j") #not lambda locals
  by_role$tail %is% c("g4", "g3", "g1")
  by_role$wind %is% c("wu")
  by_role$tramp %is% c("g3", "g4")
  by_role$util %is% c("ute")
  by_role$var %is% c("unknownConst")

  with_names <- \(x)mapply(list, names(x), x, SIMPLIFY=FALSE, USE.NAMES = FALSE)

  ntfg <- all_names(f, forGraph=TRUE, nonTail=TRUE)
  expect_setequal(
    with_names(ntfg),
    with_names( list(
      tailcall = alist(g4(temperature)),
      tailcall = alist(g3()),
      tailcall = alist(g1(temp)),
      windup = alist(wu(), g5(wu, g3)),
      handler = alist(g5(), g5(wu, g3)),
      trampoline = alist(g3(), g5(wu, g3)),
      handler = alist(g2(), g2(g4, 12)),
      trampoline = alist(g4(val = 12), g2(g4, 12)))))

})

test_that("all_names ignores tailcalls into args", {

  yielded <- NULL
  state <- NULL
  cont <- function(val) NULL
  pause_val <- function(val) NULL
  f <- function(cont, val) {
        trace("generator: yield\n")
        state <<- "yielded"
        yielded <<- val
        pause_val(cont, val)
  }

  all_names(f, forGraph=FALSE) %is% c(arg="cont", arg="val", call="trace",
                                      store="state", store="yielded",
                                      tail="pause_val")

})

test_that("all_names recognizes trampolines", {

  cont <- function() NULL
  trample <- function(cont, val) NULL
  y <- function(val) {
    force(val)
    trace("yield\n")
    yield(val) # these are different calls because make_async
    # wraps around make_pump and we affect state in both...
    trample(cont, val)
  }
  an <- all_names(y, forGraph=TRUE)
  an$handler %is% alist(trample(), trample(cont, val))
  an$trampoline %is% alist(cont(val=val), trample(cont, val))
  an$tailcall %is% NULL

  shh <- function(val) NULL
  z <- function(err) {
    trace(paste0("pump: stop: ", conditionMessage(err), "\n"))
    err <<- err
    action <<- "stop"
    shh(err)
  }
  #stop is not a trampoline call bc no "cont"
  an <- all_names(z, forGraph=TRUE)
  an$tailcall %is% alist(shh(err))
  an$trampoline %is% NULL
  an$handler %is% NULL

  pause <- function(cont) NULL
  w <- function(val, cont) {
    trace("generator: yield\n")
    yielded <<- val
    state <<- "yielded"
    pause(cont)
  }
  #"cont" is not registered as a trampoline/tailcall because it's indirect
  #(i.e. cont is in the args)
  an <- all_names(w, forGraph=TRUE)
  an$handler %is% alist(pause(), pause(cont))
  an$trampoline %is% NULL
  an$tailcall %is% NULL

})

test_that("all_names and args", {

  cont <- function(val) NULL
  x <- NULL
  R_ <- function() {
    trace(paste0("R: ", deparse(expr(x)), "\n"))
    set_dots(environment(), x)
    cont(...)
  }

  x <- all_names(R_)
  x[names(x) %in% c("read", "store", "var")] %is% c(read="x", read="x", var="...")

  all_names(function(x)x <<- x) %is% c(arg="x", store="x")

  ifTrue <- function() NULL
  all_names(function(val) {
        if(val) ifTrue() else cont(invisible(NULL))
  }, forGraph=TRUE) %is%
    list(tailcall=alist(ifTrue()),
         tailcall=alist(cont(invisible(NULL))))

  state <- NULL
  cont <- function() NULL
  pause_val <- function() NULL
  all_names(function(cont, val) {
    state <<- "yielded"
    pause_val(cont, val)
  })

})

test_that("walk() should get past a repeat in tryCatch", {

  g <- gen({
    tryCatch(
      repeat
      yield(1)
     ,
      error = function(e) NULL)
    yield("!")
  }, compileLevel=0)

  graph2 <- walk(g, forGraph=TRUE)

  expect_true(".{2.yield.R__eval_" %in% names(graph2$nodes))

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

  expect_silent(drawGraph(genprimes, filename("genprimes")))

})

test_that("tryCatch", {

  # hmm. Where is yield("!")? Node .{2.yield.R__eval_.
  # It's in the graph.
  # But it's not in nodeEdgeProperties?
  # For that matter neither is getErrHandler...
  # The compiled generator works, too.
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

  expect_silent(drawGraph(fizztry, filename("fizztry")))

})

test_that("nextOr", {

  x <- iseq(1, 55)
  incomplete <- gen(split_pipes=TRUE, {
    repeat {
      sum <- 0
      for (i in 1:10) {
        sum <- nextOr(x, {yield(sum); return()}) + sum
      }
      yield(sum)
    }
  })

  expect_silent(drawGraph(incomplete, filename("incomplete")))
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
  expect_silent(drawGraph(collatz11, filename("collatz11")))

})

test_that("yieldFrom", {

  gchain <- function(its) { force(its)
    gen(for (it in its) yieldFrom(it))
  }
  achain <- gchain(list(c("a", "b", "c"), c(1, 2, 3)))
  expect_silent(drawGraph(achain, filename("achain")))

  achain2 <- gen(for (it in its) for (i in it) yield(i))
  expect_silent(drawGraph(achain2, filename("achain2")))

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
  expect_silent(drawGraph(tryfin, filename("tryfin"),
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
  expect_silent(drawGraph(fizz, filename("fizz"),
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
  expect_silent(drawGraph(fizzbuzz, filename("fizzbuzz"),
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
  expect_silent(drawGraph(nicebuzz, filename("nicebuzz"),
                          handlers=TRUE, vars=TRUE, envs=FALSE))

})


test_that("graph of switch with goto", {

  gotoN <- gen({
    switch(x,
           yield("one"),
           goto(1),
           goto(5),
           yield("four"),
           goto(4))
    yield("done")})

  expect_silent(drawGraph(gotoN, filename("gotoN"),
                          handlers=TRUE, vars=FALSE, envs=TRUE))

  gotoChar <- gen({
      switch(x,
             one=yield(1),
             two=goto("one"),
             three=goto("five"),
             four=goto("somewhere else"),
             five=yield(5),
             yield("many"))
      yield("done")
  })

  expect_silent(drawGraph(gotoChar, filename("gotoChar"),
                          handlers=TRUE, vars=TRUE, envs=FALSE))

})

test_that("stream with on.exit", {

  ch <- mock_channel()
  fizzFilter <- stream({
    on.exit({
      yield("and that's all!")
    })
    for (i in ch) {
      if(i %% 3 == 0) {
        if (i %% 5 == 0) {
          yield("FizzBuzz")
          on.exit(yield("bonus!"))
        } else {
          yield("Fizz")
        }
      } else if (i %% 5 == 0) {
        yield("Buzz")
      }
    }
  })

  expect_silent(drawGraph(fizzFilter, filename("fizzFilter"),
                          handlers=TRUE, vars=TRUE, envs=TRUE))

})
