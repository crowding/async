#' @import nseval


## Async generators??

# What do javascript / rust / python do when it comes to making
# sequences of values?  Is this a promise which returns a result and
# optionally another promise?  Can we make it a generator?

# in JS, stuff after the final "yield" happens in the event loop (of
# whatever process runs the yield) (my JS tutorial calls this the
# "microtasks queue." The R promises package handles async processing
# in the event loop.

# How to set timeouts in an aysnc? sys.sleep() sets the whole thing.
# awaitSecs()???

# In an async generator you use both "yield" and "await" etc.
tick <- function(from, to, interval) asyncGen({
  for (i in from:to) {
    await(timeout(1000))
    yield(i);
  }
})

# With the "yield" it pauses until something else has requested the

# In Javascript, an async iterator's ?next function returns a promise,
# with a value and a "done"/not done" flag. What happens in Javascript
# if you call "next" twice without resolving the promise? It seems to
# stack up the requests.

# In Javascript an async generator exposes an iterator interface that
# yields a sequence of promises. You can request as many of these
# promises as you want! But they will be fulfilled one after another.
# Which raises the question of how this queue is maintained.

# Wouldn't the interface be better as a Promise that resolves to a
# value and the next promise?

# Or -- that's the internal interface, and the abstractAsyncGenerator
# wrapper puts the iterator view over it.

# Should "for" loops automatically do await over async generators?
for (x in range(1:5)) {
  ...
}
# I'm inclined to say no, but what's a clean alternative?
for (x in awaitAll(asyncIter)) {
  ...
}
# would be nice, but how do I create the illusion of an async turning
# into an (apparent) iterator?

# However, if you "yield" something that is a promise or an asyncGen, it
# should NOT unpack. Is this consistent?

# maybe we would have to make an asyncIterator interface, with
# implementations of map, filter, etc for generators and async
# generators.
map.asyncGen <- function(iter, f) {
  asyncGen({
    for (x in awaitAll(iter)) {
      yield(f(x))
    }
  })
}
