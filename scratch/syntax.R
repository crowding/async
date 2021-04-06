#' @import nseval

# right now I am mocking up syntax while I read about Javascript promises.

# I guess one question is whether I use promises or futures for this.
# Promises are built on futures in R.

# What do javascript or rust do when it comes to making sequences of values?
# Is this a promise which returns a result and optionally another promise?
# Can we make it a generator?
# A multiple consumer job queue would be cool too.

# * Stuff after the final "yield" happens in the event loop (of whatever
# process runs the yield) (my JS tutorial calls this the "microtasks
# queue."

# (is this true for generators as well?) Can't be, begaus you have to
# get through a generator to get the stopIteration. So "yield" in an
# async block puts any remaining into the event queue (in CPS maybe
# this is stuff after return that isn't null)?

# Now how do you poll the event queue in R?

test_that("async(function()... ) makes a function", {
  # Or maybe we should just write function() async(...)? That seems
  # consistent with what generator already does.
  x <- async(function(x) {
    return(1);
  })
}


# what would you call an asynchronous generator?
test_that("async() with something not a function def makes an immediate promise", {
  x <- async({
    return(1);
  })
})

# sleep() inside an async wets a timeout.

# Inside an generator, a "for" loop works normally.
# Also inside an async, a "for(await(i) in x)" loop should work
# with an async iterator, or maybe "for" just unpacks it anyway.

# In an async generator you use both "yield" and "await" etc.
tick <- function(from, to, interval) asyncGen({
  for (i in from:to) {
    await(timeout(1000))
    yield(i);
  }
})

# "for" loops inside of async generators and async blocks could handle
# async seamlessly? If you are writing an async() or asyncGen(), then you
# can "for" over an asyncGen and it should await on unpacking, transparently.
for (x in range(1:5)) {

}

# In Javascript, an async iterator's ?next function returns a promise,
# with a value and a "done"/not done" flag. What happens in Javascript
# if you call "next" twice without resolving the promise?

# Wouldn't you rather have a Promise that resolves to a value and
# another promise?

# Does iterables/futures package have an asyncIterator?
# However, if you "yield" something that is a promise or an asyncGen, it
# should NOT unpack. Is this consistent?

# maybe we should have implementations of map, filter, etc for
# generators and async generators.
map.asyncGen <- function() {
  asyncGen({
    for (x in tick(1:5, 1000)) {
      yield(f(x))
    }
  })
}

# Read examples in the promises/futures package and translate them to
# async/await code.  What about a syntax like
filter.asyncGen <- function(in, f) asyncGen({
  if (await(iHasNext(in))) {
    yield(f(await(next(in))))
  }
})

# Idea: support return() and resolve(). The diff is that return
# doesn't do any "cleanup" while "resolve" should still execute the cleanup.

# question: what if we resolve() returning? a promise itself? Should
# promises nest or collapse? I'm inclined to say they should not unwrap.

while(await(iHasNext(x)))
  # Okay here's a syntax question, do we like "yield" or "return"?
  x <- async(tryCatch({
    file <- await(open(file))
    return(await(read(file)))
  }, finally=close(x)))

y <- async(tryCatch({
  file <- await(open(file))
  yield(await(read(file)))
  close(file)
})

# Question: Do promises resolve on their own even if not actively
# polled (does the event loop poll them?) I would suppose it has to,
# to support timeouts.

# async/await works with "thenable" objects (i.e. those with the method
# then(resolve, reject))

# hey why not look at promises/futures R test cases or example functions and translate those.

# notes from r "promises" package

# convention for functions returning promises is name them ending with ".async"
# consider automadically expanding read.csv(x) into await(read.csv.async(x))) ???
# probably too magic.

# make sure that async() can be use in a "reactive expression" in Shiny. That is,

filtered_df <- reactive(async({
  x <- await(read.csv.async("https://rstudio.github.io/promises/data.csv"))
  x %>% filter(state == "NY") %>% arrange(median_income)
}))


# promise rejections coming from "await" should bubble into
# exceptions, and exceptions occurring in async should emerge as exceptions.


test_that("now what happens when there is an error in the finally/cleanup?")

test_that("await", {
  x <- async(return(1));
  y <- async(return(2));
})

test_that("await")

test_that("await works on futures/promises/thenables objects", {

})

          # In Javascript, a .then/.catch/finally executs asynchronously.  How
          # to make this happen in async/await? The "finally" might just be whatever follows a yield...
          # Seems like there would need to
          # be a detection of what the "finally" is, then return, asynchronously.

          # In a regular promise there is a yield....

          # So let's collect some equivalent things in R promises/futures and async/await.

          # It seems like there is a lot of complexity in how. However I think
          # tryCatch in a generator is straightforward enough, or
          # withCallingHandlers, etc.

          # Throw should reject?

          # the argument for Promise is the continuation function. So this is
          # just continuation passing style?

          ?tryCatch

          # await all promises:

          # cencelable promises

          # try/finally in generators and promises.
          # Perhaps I should pass handlers down alongside the continuation function?
          # async(tryCatch(..., yield(y), finally=log("Done!"))) should...
          # Does there need to be an event queue for finally-resolution?

          x <- await(promise_all())

          # handler for externally aborting a promise?

          # Promises have a state, unresolved, resolved, or rejected.

          test_that("async/await", {
            # an "async" block creates a promise, which can yield 
            x <- async({
              yield(12)
            })
            await(x)
          })

          test_that("yield nothing allows you to poll", {
            times <- 0
            x <- async({
              for(i=1:10) {
                times <<- times + 1 #note there is a 
              }
              yield(1)
            })
          })

          test_that("await going between two async blocks", {
            x <- async({
              await(y) + 1 #implicit yield from end
            })

            y <- async({
              yield() # yield() empty is a no-op
              yield(5)
            })
          })

          test_that("generator that returns promises", {
            x <- gen({
              yield(async(""))
              yield()
              yield(for=0.5) # seconds
              yield(until=) # wait until a certain time (POSIXct, )
            })
          })

          test_that("await on a settled promise is a no-op", {
            
          })

          test_that(x <- gen_async(), {
            # "next" and "await" do the same here with a gen_async
            # within a gen_async, "yield" returns???
          })


          #


          
