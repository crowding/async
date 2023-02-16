# async 0.3

Version 0.3 of `async` contains a number of new features, usability and performance improvements.

* Coroutines now support single step debugging. Use `debugAsync(gen, TRUE)` to open a browser at each point the coroutine reaches an R expression. Use `debugAsync(gen, internal=TRUE)` to step through at a lower level, inside the `async` package implementation.
  
* There is now a function `goto(branch)` that works inside of `switch` statements, allowing you to jump to another branch; while `goto()` with no arguments will jump back re-evaluate the switch argument.

* Coroutines now support exit handlers; handlers are registered with `on.exit` and will be run when the coroutine reaches the end of its code, either normally or by error.

* There is now an experimental implementation of `channel` datatype, which is like a combination of an iterator and a promise; a channel represents a sequence of values yet to be determined. You can treat a channel as an iterator that returns a series of promises, or to be slightly more streamlined you can use `nextThen(onNext, onError, onClose)`, providing callbacks to receive the next value. An `async` can retrieve values from a channel using `awaitNext` or a `for` loop. The implementation of channel includes queues on both input and output, which allows flexibility for different types of data sources.

* To go along with channels there is now a `stream` coroutine. A stream implements the `channel` interface, and in its expression you can use _both_ `await` and `yield`. Additionally a stream can com in two flavors, `lazy=TRUE` (where the stream begins in a paused state, and will not do anything until it has at least one listener request) and `lazy=FALSE` (the stream starts executing immediately, and will continue executing after `yield`, even with no listeners, queuing up output values until it reaches an `await`). For comparison, this package's  `gen` coroutines are lazy in this sense, while `async` are eager.

* Generators are now displayed with a label corresponding to where in their code they were last paused. The function `getNode(g)` queries this label. For example:

```
    > g <- gen(for (i in 1:10) {yield(i); if (x %% 2 == 0) yield("even.") else yield("odd!")})
    > g
    gen(for (i in 1:10) {
    yield(i)
    if (x%%2 == 0) 
        yield("even.")
    else yield("odd!")
    })
    <environment: R_GlobalEnv>
    <Generator [yielded at `.for2.R__eval_`]>
    > nextElem(g)
    [1] 1
    > getNode(g)
    [1] ".for3.{1;__;"
    > nextElem(g)
    [1] "odd!"
    > getNode.generator(g)
    [1] ".for__again"
```

  To unpack the above, on creation the generator is paused at node `".for2.R__eval_"` i.e. the R expression in the second argument of `for`, which in this case is the expression `1:10`. After executing the first `yield`, the generator shows it is paused at `".for3.{1;__;"`, that is, at the "semicolon" following the first statement in the braces in the third argument of `for`; so the next thing the generator will execute is the subsequent `if` statement. After the next `yield`, the label `".for__again"` means that the generator's next action will be to return to the beginning of the `for` loop.

* Under the hood, the implementation has been refactored heavily in order to enable compilation; the package now includes the back half of a compiler. To create a compiled generator you can write `gen({...}, compileLevel=-1)`; this will take a bit of CPU time but shouldn't change the functionality. (Or speed; making it faster will be the job of the front half.)

* A side benefit of compilation work is that coroutines can draw a picture of their graph structure,  `drawGraph(myGen)` collects a coroutine's structure and writes a Graphviz DOT file (which by default would be named `myGen.dot` in the current directory.)  If the Graphviz `dot` command is visible on your `$PATH` it will then be run to render a PDF file.

* There is now a syntax for "generator functions" (as well as async functions, and stream functions). If the argument is a function expression, `gen` will construct a function that constructs a generator. So these two calls are nearly equivalent:

```
    g <- gen(function(x, y) for (i in x:y) yield(i))
    g <- function(x, y) {force(list(x, y)); gen(for (i in x:y) yield(i))}
```

(so really it just saves you from remembering to `force` your arguments.)

# async 0.2.2

  * Re-generate documentation for R-devel

# async 0.2.1

Fixes:

  * Namespacing issue in test vs check

# async 0.2

Changes:

  * Generators and asyncs work in a localy created scope.
  * `tryCatch` for error handling
  * `split_pipes` helps use `await` in pipelines
  * Cranwhales Shiny app demo
  * `trace` option for logging of async operations
  * `pausables` lists all pausable functions
  * `delay` for sleeping an `async`

# async 0.1

Changes:

  * Rename from "generators" to "async"
  * Async/await blocks implementing the promise interface 
  * Performance improvements from building the call graph ahead of time

# generators 0.0.0.9000

  * Initial Github release.
