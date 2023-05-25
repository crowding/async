# make up some fake graphs and see that they inline properly

getStartSet.list <- function(obj, ...) obj
summary.list <- function(obj, ...) list(code=quote({}))

# dynamically register just for the sake of this test...
s3_register("async::getStartSet", "list", getStartSet.list)
s3_register("base::summary", "list", getStartSet.list)

toy_graph <- function() {
 storage <- NULL
  state <- NULL
  CONSTANT <- TRUE
  .contextName <- "test"
  globalNode(node1 <- function() {
    val <- 1
    store(node2, val)
  })
  globalNode(store <- function(cont, val) {
    storage <<- val
    cont(val)
  })
  globalNode(node2 <- function(val) {
    val <- NULL
    node3()
  })
  globalNode(node3 <- function() {
    if(CONSTANT) {
      bounce(node1)
    } else {
      bounce(node1)
    }
  })
  globalNode(bounce <- function(cont) {
    state <<- cont
  })

  walk(list(node1=node1))
}

test_that("walk a simple graph", {

  graph <- toy_graph()

  expect_equal(graph$contextNodes$test, list(
    node1="node1", node2="node2", node3="node3",
    bounce="bounce", store="store"))

  graph$contextProperties$test$read %is% c("CONSTANT")
  graph$contextProperties$test$store %is% c("state", "storage")
  graph$contextProperties$test$hand %is% c("store", "bounce")
  graph$contextProperties$test$tramp %is% c("node2", "node1")
  graph$contextProperties$test$tail %is% c("node3")

  graph$nodeEdgeProperties$node1$store$type %is% "hand"
  graph$nodeEdgeProperties$node1$node2$type %is% "tramp"
  graph$nodeEdgeProperties$node2$node3$type %is% "tail"
  graph$nodeEdgeProperties$node3$bounce$type %is% "hand"
  graph$nodeEdgeProperties$node3$node1$type %is% "tramp"

  graph$reverseEdges$store$node1 %is% "store"
  graph$reverseEdges$bounce$node3 %is% "bounce"
  graph$reverseEdges$node1$node3 %is% "node1"
  graph$reverseEdges$node2$node1 %is% "node2"
  graph$reverseEdges$node3$node2 %is% "node3"

})

test_that("partial if statement", {

  graph <- toy_graph()
  inline_expr(quote(CONSTANT),
              graph, nodeName = "node3", FALSE) %is%
    TRUE
  inline_expr(quote(if (FALSE) return(1)),
              graph, nodeName = "node3", FALSE) %is%
    quote({})
  inline_expr(quote(if (CONSTANT) foo(1) else foo(2)),
              graph, nodeName = "node3", FALSE) %is%
    quote(foo(1))
  inline_expr(quote(if (!CONSTANT) this else that),
              graph, nodeName = "node3", FALSE) %is%
    quote(that)
  inline_expr(quote(if (TRUE) if (FALSE) 1 else 2 else 3),
              graph, nodeName = "node3", FALSE) %is%
    quote(2)
  inline_expr(quote(if (CONSTANT || storage) foo else bar),
              graph, nodeName = "node3", FALSE) %is%
    quote(foo)
  inline_expr(quote(if (!CONSTANT || storage) foo else bar),
              graph, nodeName = "node3", FALSE) %is%
    quote(if(storage) foo else bar)
  inline_expr(quote(if (CONSTANT && storage) foo else bar),
              graph, nodeName = "node3", FALSE) %is%
    quote(if(storage) foo else bar)
  inline_expr(quote(if (!CONSTANT && storage) foo else bar),
              graph, nodeName = "node3", FALSE) %is%
    quote(bar)
  inline_expr(quote(if (state == 5) foo else bar),
              graph, nodeName = "node3", FALSE) %is%
    quote(if (state == 5) foo else bar)
  inline_expr(quote(if (CONSTANT == 5) foo else bar),
              graph, nodeName = "node3", FALSE) %is%
    quote(bar)
  inline_expr(quote(if (!CONSTANT != TRUE) foo else bar),
              graph, nodeName = "node3", FALSE) %is%
    quote(foo)
  inline_expr(quote(if (CONSTANT != state) foo else bar),
              graph, nodeName = "node3", FALSE) %is%
    quote(if(TRUE != state) foo else bar)

})

test_that("partial handler", {

  graph <- toy_graph()
  inline_expr(quote({val <- 5; bounce(node2)}),
              graph, nodeName="node3", TRUE) %is%
    quote({val <- 5; state <<- node2})

})
