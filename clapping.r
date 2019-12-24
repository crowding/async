# This is a tutorial for the generators package. To install the
# package, run:
#
## devtools::install_github("crowding/generators")
#
# Then take this file and evaluate step by step in an R interpreter.

####### Generators for R

# What a generator allows you to do is to take code that is writen in
# a sequential, branching style, and then treat that code's output as
# a collection to be iterated over.

# To illustrate what this all means, and the the generators package in
# general I will use R to perform a suitable piece music,
# specifically Steve Reich's "Clapping Music."

# The basic repeating motif in 'Clapping Music' is a loop of three
# claps, then two, then one, then two, then three again, and so on;
# each group seperated by one space. We might write it thus.

print_pattern <- function(counts = c(3, 2, 1, 2)) {
  for (n in counts) {
    for (j in 1:n)
      print("clap")
    print("")
  }
}
for (i in 1:4) print_pattern()

# Now print statements don't make for data that can be plugged into
# other functions. But we can make this a generator, by adding a small
# wrapper.
pattern <- function(counts = c(3, 2, 1, 2)) { force(counts)
  generators::gen({
    for (n in itertools::recycle(counts)) {
      for (j in 1:n)
        yield("clap")
      yield("")
    }
  })
}

# The code inside "gen" does not run, yet. The "gen" constructs an
# iterator which supports the method `nextElem` from the iterators
# package. When `nextElem()` is called on a generator, it runs its
# code only up to the point where "yield" is called, then saves its
# state until the next call to `nextElem()`.

p <- pattern()
cat(nextElem(p), "\n")
cat(nextElem(p), "\n")
cat(nextElem(p), "\n")
for (i in 1:23) {
  cat(nextElem(p), "\n")
}

# Note the appearance of `itertools` in the for loop. In a generator
# you can go ahead and put an iterator (or any type with an `iter`
# method) into the for-loop argument, which outside of a generator
# would require use of the "foreach" package. This means that a
# for loop is running over an indefinitely long sequence.

# To show the first few outputs of this indefinite process, we can use
# iterator functions to collect its output:
pattern() %>% itertools::ilimit(24) %>% as.list %>% deparse %>% cat(sep="\n")

# I'll catch that last bit of pipeline in a function to reuse.
show_head <- function(x, n=24) {
  x %>% itertools::ilimit(n) %>% as.list %>% deparse() %>% cat(sep="\n")
}

# https://github.com/octoblu/drum-kit/raw/master/public/assets/samples/clap%20(1).wav

# We'll need some clap samples. I found these on Github...
curl::curl_download("https://github.com/octoblu/drum-kit/raw/master/public/assets/samples/clap%20(3).WAV", "clap3.wav")
curl::curl_download("https://github.com/octoblu/drum-kit/raw/master/public/assets/samples/clap%20(5).WAV", "clap5.wav")

# Let's see about making these loops add sound. `with_sound` takes
# in a generator and returns a new generator one which plays a sound
# after processing each item.
with_sound <- function(g, path) { list(g, path)
  sample <- audio::load.wave(path)[1:15000]
  generators::gen(
    repeat{
      x <- nextElem(g)
      if(x != "") audio::play(sample)
      yield(x)
    }
  )
}

# So these should play a sound:
gen(yield("hi")) %>% with_sound("clap3.wav") %>% nextElem
gen(yield("hi")) %>% with_sound("clap5.wav") %>% nextElem

# Two small other pieces we will need are a way to map a function over
# an iterator, and a way to just consume all the elements from an
# iterator and return nothing. First the map:
iter_map <- function(it, f, ...) { list(it, f, ...)
  itertools::new_iterator(function() f(iterators::nextElem(it), ...))
}
# Which with generators we could also write:
iter_map <- function(it, f, ...) { list(it, f, ...)
  generators::gen(for (x in it) yield(f(x, ...)))
}

# And a function to just consume an iterator and return nothing:
sink <- function(i) {
  tryCatch(
    repeat nextElem(i),
    error=function(e) {
      if (identical(conditionMessage(e), "StopIteration"))
        invisible(NULL)
      else
        stop(e)
    })
  invisible(NULL)
}
# which we could also write thus:
sink <- function(i) {
  g <- generators::gen({
    for (x in i)
      FALSE && yield(NULL) # gen() expects there to be a yield() even
                           # if it is never called.
  })
  invisible(as.list(g))
}

# Now, back to the topic of music.  If we let the generators run at
# fast as they could, , the sound played wouldn't be intelligible. So
# here is a 'tempo' generator function -- it passes its input to its
# output at a specified rate.
with_tempo <- function(g, bpm) {  list(g, bpm)
  interval <- 60 / bpm
  generators::gen({
    target <- Sys.time()
    for (i in g) {
      yield(i)
      target <- target + interval
      now <- Sys.time()
      if (target - now > 0.15) Sys.sleep(target - now - 0.15)
      profvis::pause(target - Sys.time()) #for better precision
    }
  })
}
# So this should print numbers at a regular interval:
iter(1:20) %>% with_tempo(180) %>% iter_map(cat) %>% sink

## Now we should be able to plug out noisemaker into our metronome.
pattern() %>% with_sound("clap3.wav") %>%
  with_tempo(360) %>% itertools::ilimit(48) %>% sink

# "Clapping Music" is a piece for two performers, though, but that's fine:
# we can keep two noisemakers in step with `izip`.

itertools::izip(pattern() %>% with_sound("clap3.wav"),
                pattern() %>% with_sound("clap5.wav")) %>%
  with_tempo(280) %>% itertools::ilimit(12) %>% sink

# The trick with "Clapping music" is that after 12 bars, one of the
# performers jumps ahead by one beat, so they play out of sync.
# Generators offer an intuitive way to write this:
drop_one_after <- function(g, n) {  list(g, n) #force
  generators::gen(
    repeat {
      for (i in 1:n) yield(iterators::nextElem(g))
      iterators::nextElem(g)
    }
  )
}
# Here, illustrated by dropping one after emitting two -- that is,
# dropping every third item.
iterators::icount() %>% drop_one_after(2) %>% show_head

# [[ Site note / caution. By now you might have noticed I've been
# placing "force" and "list" in the first line of functions taht
# return generators.  This is actually something you should do any
# time you construct and return an inner function, iterator, promise,
# or other construct that captures your arguments for future
# reference. The issue is the basic R issue, where lazy evaluation +
# mutable bindings allows a function's inputs to change after it has
# been called -- for example, `%>%` will call a pipeline function with
# the argument `.`, but after it returns, `%>%` will change the
# binding for `.` A `for` loop similarly clobbers the binding of its
# iterator variable. ]]

# Now we are ready to put it all together -- two overlapping rotating
# patterns, running as independent loops, that we combine and
# interleave.

clapping_music <- function(n=12) {
  itertools::izip(
    pattern(c(3,2,1,2)) %>% with_sound("clap3.wav"),
    pattern(c(3,2,1,2)) %>% drop_one_after(12*n) %>% with_sound("clap5.wav")
  ) %>%
    itertools::ilimit(13*12*n) %>%
    itertools::ilimit(13*12*n) %>%
    with_tempo(280)
}

# To narate this a bit: we are constructing two independent instances
# of our basic 12-beat XXX.XX.X.XX. pattern. One of these patterns is
# made to skip forward one beat every N bars. These two loops are then
# combined in parallel with "izip" then "paste", then there is the
# tempo limiter. In Steve Reich's original spec each bar is repeated 12
# times, but we use for a shorter version.
clapping_music(4) %>% sink()

# R is definitely not a multimedia environment, and I can hear some
# glitches and pauses on occasion. However this does illustrate how
# the generators package allows control to be _interleaved_ among
# different sequential processes. The generator package lets you
# escape from obligatory cellection-thinking, to express with
# sequential code that which is best represented that way. and work at
# the level of collections and iterators when that is most convenient.

# Peter Meilstrup
# Dec. 23, 2019

