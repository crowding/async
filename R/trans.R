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

# this one works with environments, would be faster if translated to C?
trans2 <- function(expr, callTranslations, varTranslations
                  #, keep.source=FALSE ## TODO
                  ) {
  S <- function(expr, head=FALSE) {
    switch(
      mode(expr),
      name = {
        n <- as.character(expr)
        if (head) mget(n, envir=callTranslations, ifnotfound=list(expr))[[1]]
        else mget(n, envir=varTranslations, ifnotfound=list(expr))[[1]]
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
  }
  S(expr, FALSE)
}

translationEnv <- function(callTranslations, varTranslations) {
  # We need something slightly more sophisticated than substitute()
  # because we need to ignore names attached to "::",
  # arguments of quote(), etc. I'm using a trick of creating an
  # environment in which I can eval the normal expression and get
  # the translated expression back.
  # However, this technique can't properly deal with more
  # complicated call heads.
  ignored_call_head <- function(...) {
    # skip over "::" in the head of calls
    x <- match.call();
    function(...) as.call(c(list(x), list_missing(...)))
  }
  ignored_call <- function(...) match.call()
  callenv <- as.environment(
    list(
      `::` = ignored_call_head,
      `:::` = ignored_call_head,
      `quote` = ignored_call
    ))
  nameenv <- new.env(parent=callenv)
  idempotize(nameenv, names=varTranslations, callnames=callTranslations)
  nameenv
}

translateWithEnv <- function(expr, tenv) {
  idempotize(tenv, all.names(expr))
  eval(expr, tenv)
}

idempotize <- function(nameenv, names,
                       callenv=parent.env(nameenv), callnames=names) {
  if (is.null(names(names))) names(names) <- as.character(names)
  if (is.null(names(callnames))) names(callnames) <- as.character(callnames)

  #using mget for a side effect of calling ifnotfound
  mget(
    names(names), nameenv, inherits=TRUE,
    ifnotfound=list(function(n) {
      if (n == "...") {
        set_dots(nameenv, dots(quote(...), envir=nameenv))
      } else {
        assign(n, as.name(names[[n]]), envir=nameenv)
      }
    }))

  mget(
    names(callnames), callenv, inherits=TRUE,
    ifnotfound=list(function(n) {
      f <- function_(
        list(...=missing_value()),
        substitute(
          as.call(c(quote(x), list_missing(...))),
          list(x=as.name(callnames[[n]]))),
        environment())
      assign(n, f, envir=callenv)
    }))
}
