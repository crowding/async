# I need something slightly more sophisticated than substitute(),
# but alas it is noticeably slower...
trans <- function(expr, callTranslations, varTranslations
                  #, keep.source=FALSE ## TODO
                  ) {
  S <- function(expr, head=FALSE) {
    switch(
      mode(expr),
      name = {
        n <- as.character(expr)
        if (head && (n %in% names(callTranslations))) {
          as.name(callTranslations[[n]])
        } else if (!head && (n %in% names(varTranslations))) {
          as.name(varTranslations[[n]])
        } else {
          expr
        }
      },
      call = switch(
        mode(expr[[1]]),
        `character` = ,
        `name` = switch(
          as.character(expr[[1]]),
          # ignored call
          `::` =, `quote` = expr,
          #regular call
          as.call(c(list(S(expr[[1]], TRUE)), lapply(expr[-1], S, FALSE)))
        ),
        # default: call w/an expression as head
        as.call(c(list(S(expr[[1]], TRUE)), lapply(expr[-1], S, FALSE)))),
      #default
      expr
    )
    ## cat(deparse(substitute(expr %->% result,
    ##             list(expr=expr, result=result))), "\n")
  }
  S(expr, FALSE)
}

transX <- function(expr, callTranslations, varTranslations
                  #, keep.source=FALSE ## TODO
                  ) {
  collect_tree(function(yield, open, close) {
    visit <- function(expr, head, name) {
      switch(
        typeof(expr),
        symbol = {
          n <- as.character(expr)
          if (head && (n %in% names(callTranslations))) {
            yield(as.name(callTranslations[[n]]), name)
            } else if (!head && (n %in% names(varTranslations))) {
              yield(as.name(varTranslations[[n]]), name)
          } else {
            yield(expr)
          }
        },
        language = {
          switch(
            mode(expr[[1]]),
            `character` = ,
            `name` = switch(
              as.character(expr[[1]]),
              # ignored call
              `::` =, `quote` = {
                yield(expr); return()
              })
          )
          #regular call OR call with call in head
          open()
          if (length(expr) >= 1) {
            visit(expr[[1]], TRUE, names(expr)[[1]])
            for (i in seq_len(length(expr)-1)+1)
              visit(expr[[i]], FALSE, names(expr)[[i]])
          }
          close("language", name, attrs=attributes(call))
        },
        #default
        yield(expr, name)
      )
    }
    visit(expr, TRUE, NULL)
  })[[1]]
}
