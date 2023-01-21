# async 0.3

Version 0.3 of `async` contains a number of features, usability and performance improvements.

* Coroutines now support single step debugging. Use `debugAsync(gen, TRUE)` to open a browser at each point the generator reaches and evaluates R expression. Use `debugAsync(gen, internal=TRUE)` to step through at a lower level, inside the `async` package implementation.
  
* There is now a delimited `goto` that works inside of `switch` statments.

* Coroutines now support `on.exit` handlers.

 * Generators are now displayed with a label corresponding to where in their code they are paused. The function `getNode(g)` queries and computes this label. For example:

```
    > g <- gen(for (i in 1:10) {yield(i); if (x %% 2 == 0) yield("odd!") else yield("odd!")})
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

To unpack the above, on creation the generator is paused at node `".for2.R__eval_"` i.e. the R expression in the second argument of "for"; which in this case is the expression `1:10`. After executing the first yield, the generator shows it is paused at `".for3.{1;__;"`, that is, at the "semicolon" following the first statement in the braces in the third argument of `for`; so the next thing the generator will execute is the subsequent "if" statement. After the next `yield`, the label `".for__again"` shows the generator's next action will be to return to the beginning of the `for` loop.

* Under the hood, the implementation has been refactored with the hope of enabling compilation; the package now includes the back half of a compiler. You can write `gen({...}, compileLevel=-1)`; this will take a bit of CPU time but shouldn't change the functionality eny (makign it faster is the front half of a compiler.

  A side benefit of this is that coroutines can draw a picture of their graph structure,  `drawGraph(myGen)` collects a coroutine's graph structure and writes a Graphviz DOT file (which by default would be named `myGen.dot` in the current directory.)  If the Graphviz `dot` command is visible on your `$PATH` it will be invoked to render a PDF file.
  
* There is now an experimental implementation of `channel` datatype, which is like a combination of an iterator and a promise; a channel represents a sequence of values yet to be determined. An `async` can retrieve values from a channel using `awaitNext` or a `for` loop.

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
