# a generator function can be called from outside like a normal function
function(n) gen({
  for (i in 1:n) {
    yield(i)
  }
})

