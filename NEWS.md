# async 0.3

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

  Upon creation the generator is paused at node `".for2.R__eval_"` i.e. the R expression in the second argument of "for"; which in this case is the expression `1:10`. After executing the first yield, the generator shows it is paused at `".for3.{1;__;"`, that is, at the "semicolon" following the first statement in the braces in the third argument of `for`; so the next thing the generator will execute is the subsequent "if" statement. After the next `yield`, the label `".for__again"` shows the generator's next action will be to return to the beginning of the `for` loop.

* A side benefit of work on a compiler, generators and asyncs can draw a diagram of their structure. `drawGraph(myGen)` collects the graph structure and writes a Graphviz DOT file (which by default would be named `myGen.dot` in the current directory.)  f you have Graphviz installed it will be run to produce a PDF file.
  
* Generators support single step debugging. Use `debugAsync(gen, TRUE)` to enable debugging at the next point the generator reaches an R expression. set `debugAsync(gen, internal=TRUE)` to debug at a lower level, inside `async` implementation.
  
* There is now a delimited `goto` that works inside of `switch` statments.

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
