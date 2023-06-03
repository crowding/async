

# would like some kind of global mechanism that keeps the calls to
# [socketSelect] to a minimum... but that's too complicated for now,
# just do later(socketSelect)


#' Using socket connections through channels
#'
#' @rdname sockets
#' @description `channel.servsockconn(obj)` takes an open server
#'   socket `obj`, as returned from [serverSocket(...)]. and
#'   constructs a channel which yields an opened [socketConnection]
#'   object for each incoming connection.
#' @exportS3Method
channel.servsockconn <- function(obj, ...) {
  # FIXME should accept arguments for socketAccept()...
  listening <- TRUE
  if (!isOpen(obj, "read")) {
    stop("Socket must be opened")
  }

  channel(\(doEmit, doReject, doClose) {
    pollSocket <- function() {
      tryCatch({
        if (socketSelect(list(obj), timeout=0)) {
          con <- socketAccept(obj, blocking=FALSE)
          doEmit(con)
          later(pollSocket)
        } else if (listening) {
          later(pollSocket, delay=0.05)
        } else {
          close(obj)
          doClose()
        }
      }, error=\(x) {
        listening <<- FALSE
        doReject(x)
      })
    }
    later(pollSocket)

  }, onClose = \() listening <<- FALSE)
}

#' @rdname sockets
#' @description `channel.sockconn(obj)` takes an open socket
#'   connection and returns a channel which yields data as it comes
#'   in.  [socketConnection] object for each incoming connection to
#'   the socket.
#' @exportS3Method
#' @param obj a [socket connection][socketConnection].
#' @param ...
#' @param read can be "lines", "obj", or "char"
#' @param read_params List of default parameters to pass to `readLines`,
#'   `readChar` or `readBin` as appropriate.
channel.sockconn <- function(obj, ...,
                               read = {
                                 if (summary(obj)$text == "text")
                                   c("lines")
                                   else c("bin", "lines", "char")
                               },
                               read_params = {
                                 switch(read,
                                        lines = list(n = 1),
                                        char = list(nchar = 1),
                                        bin = list(what = "raw", n = 1))
                               }) {
  if (!isOpen(obj, "read"))
    stop("Need to open the connection for reading before making a channel")

  read <- match.arg(read)
  readMethod <- switch(read,
                       lines=readLines,
                       char=readChar,
                       bin=readBin)

  doEmit <- identity
  doReject <- identity
  doClose <- function() NULL

  arguments <- (read_params
    |> names()
    |> lapply(as.name)
    |> structure(names=names(read_params)))

  readCall <- function_(
    c(list(...), read_params),
    bquote(splice=TRUE, {
      readMethod(obj, ..., ..(arguments))
    }),
    environment()
  )

  doRead <- function(...) { #here's where it would be nice to use socketSelect...
    poll <- function() {
      tryCatch({
        if (isOpen(obj)) {
          result <- readCall(...) # this is blocking!!!
          if (length(result) == 0) {
            # I get here when connection is closed from the other end?
            # But it's not closed from my end.
            # I guess the only way to tell if a connection is closed remotely is
            # to read from the other end???
            later(poll, 0.05)
          } else {
            if (isIncomplete(obj)) {
              browser()
              cat("incomplete results...\n")
              later(poll, 0.05)
            } else {
              cat("got something...\n")
              doEmit(result)
            }
          }
        }
      }, error=doReject)
    }
    poll()
  }

  channel(\(doEmit, doReject, doClose) {
    doEmit <<- doEmit; doReject <<- doReject; doClose <<- doClose
  }, wakeup = function(...) doRead(...))

}
