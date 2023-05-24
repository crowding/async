# So there is this memory leak... my generator objects
# are hanging on to a reference to the evaluation
# environment. How to find the reference?

# My strategy is to put a magic value into the environment, then
# serialize() to check if that value is present in the output.

nonmagic <- "123456788"

serialized_has <- function(x, pattern)
( x
  |> serialize(NULL, ascii=TRUE)
  |> rawToChar() |> strsplit("\n")
  |> grepl(x=_, pattern)
  |> any()
)

# the test object, a generator function with local=FALSE
g <- gen(function(n, len) {
  for (i in seq_len(n)) {
    x <- as.integer(nonmagic)+1L
    yield(mean(x))
  }
})

it <- g(2, "irrelevant")
it()
it()
it(NULL) # ref to working environment should be dropped but isn't
magic <- "123456789"

serialized_has(it, magic)

eapply(environment(it), serialized_has, magic)
# all the closures in the environment have a ref... which would make
# sense; if the reference is in one object in teh environment, all
# closures in that environment would be able to reach that references.
# So how about if we remove all the closures from the environment,
# then the one with the "true" reference should be easier to spot.

duped <- as.environment(as.list(environment(it)))
rm(list=names(environment(it)), envir=environment(it))
eapply(duped, serialized_has, magic)

# cool so "pump" is holding the bag. pause_val is in the same env.
# pump's environment has unforced promsxps. So as.list() will crash.
# let's try the same trick of removing all the environment's members...
e <- environment(duped$pump)
dotted <- nseval::as.dots(e)
rm(list=names(e), envir=e)
lapply(dotted, serialized_has, magic)

# okay so "pumpCont", "entry" in the pump environment.
# Their enclosing environment?
e2 <- value(dotted$pumpCont) |> environment()
dotted2 <- nseval::as.dots(e2)
rm(list=names(e2), envir=e2)
lapply(dotted2, serialized_has, magic)

e3 <- environment(value(dotted2$brk_)) |> parent.env()
duped3 <- as.list(e3)
rm(list=names(e3), envir=e3)
lapply(duped3, serialized_has, magic)

# oho what's this...
duped3$var |> environment() |> as.dots()

# an unforced promise in .contextName in async:::R

## Okay next up, async functions:
nonmagic <- "123456788"
m <- mock_promise()
af <- async(function(x) {
  value <- as.integer(nonmagic) + 1L
  await(x)
})

magic <- "123456789"
as <- af(m)
wait_for_it()
serialized_has(as, magic)
m$resolve(5)
wait_for_it()
serialized_has(as, magic)

lapply(as, serialized_has, magic)

e4 <- as$state
dots4 <- nseval::as.dots(e4)
rm(list=names(e4), envir=e4)
lapply(dots4, serialized_has, magic)

e5 <- dots4$pump |> value() |> environment()
dots5 <- nseval::as.dots(e5)
rm(list=names(e5), envir=e5)
lapply(dots5, serialized_has, magic)

e6 <- dots5$entry |> value() |> environment()
e6 |> serialized_has(magic)
dots6 <- nseval::as.dots(e6)
rm(list=names(e6), envir=e6)
lapply(dots6, serialized_has, magic)

e7 <- dots6$cont |> value() |> environment()
dots7 <- nseval::as.dots(e7)
rm(list=names(e7), envir=e7)
lapply(dots7, serialized_has, magic)

e8 <- dots7$cont |> value() |> environment()
dots8 <- nseval::as.dots(e8)
rm(list=names(e8), envir=e8)
lapply(dots8, serialized_has, magic)

# "pause" is unforced and its environment...
e9 <- dots8$pause |> env()
serialized_has(e9, magic)
dots9 <- nseval::as.dots(e9)
rm(list=names(e9), envir=e9)
lapply(dots9, serialized_has, magic)

e10 <- dots9$error |> value() |> environment() |> parent.env()
e10 |> serialized_has(magic)
dots10 <- nseval::as.dots(e10)
rm(list=names(e10), envir=e10)
lapply(dots10, serialized_has, magic)

#okay it's another unforced .contextName... but where? await_cp
names(dots10)
dots10$prom
