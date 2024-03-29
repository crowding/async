% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/channel.R
\name{channel}
\alias{channel}
\alias{channel.function}
\alias{is.channel}
\title{An object representing a sequence of future values.}
\usage{
channel(obj, ...)

\method{channel}{`function`}(
  obj,
  ...,
  max_queue = 500L,
  max_awaiting = 500L,
  wakeup = function(...) NULL
)

is.channel(x)
}
\arguments{
\item{obj}{A user-provided function; it will receive three
callback functions as arguments, in order, \code{emit(val)},
\code{reject(err)} and \code{close()}}

\item{...}{Specialized channel methods may take other arguments.}

\item{max_queue}{The maximum number of outgoing values to store if
there are no listeners. Beyond this, calling \code{emit} will return
an error.}

\item{max_awaiting}{The maximum number of pending requests. If
there are this many outstanding requests, for values, calling
\code{nextThen(ch, ...)} or \code{nextElem(ch)} will raise an error.}

\item{wakeup}{You may optionally provide a callback function here.
It will be called when the queue is empty and there is at least
one listener/outstanding promise.}

\item{x}{an object.}
}
\value{
a channel object, supporting methods "nextThen" and "nextOr"

\code{is.channel(x)} returns TRUE if its argument is a channel object.
}
\description{
A \code{channel} is an object that represents a sequence of values yet
to be determined. It is something like a combination of a \link{promise}
and an \link{iteror}.
}
\details{
The channel interface is intended to represent and work with
asynchronous, live data sources, for instance event logs,
non-blocking connections, paginated query results, reactive values,
and other processes that yield a sequence of values over time.

\code{channel} is an S3 method and will attempt to convert the argument
\code{obj} into a channel object according to its class.

The friendly way to obtain values from a channel is to use
\code{awaitNext} or \code{for} loops within an \link{async} or \link{stream} coroutine.

The low-level interface to obtain values from a channel is to call
\link{nextThen}\verb{(ch, onNext=, onError=, onClose=, ...)]}, providing callback
functions for at least \code{onNext(val)}. Those callbacks will be
appended to an internal queue, and will be called as soon as data
is available, in the order that requests were received.

You can also treat a channel as an \link{iteror} over promises, calling
\code{nextOr(pri)} to return a \link{promise} representing the next
available value. Each promise created this way will be resolved in
the order that data come in. Note that this way there is no special
signal for end of iteration; a promise will reject with
a condition message \code{"StopIteration"} to signal end of iteration.

Be careful with the iterator-over-promises interface though: if you
call \code{as.list.iteror(pr)} you may get stuck in an infinite loop, as
\code{as.list} keeps calling \code{nextElem} and receives more promises to
represent values that exist only hypothetically. This is one
reason for the \code{max_listeners} limit.

The friendly way to create a channel with custom behavior is to use
a \link{stream} coroutine. Inside of \code{stream()} call \link{await} to wait on
promises, \link{awaitNext} to wait on other streams and \link{yield} to yield
values. To signal end of iteration use \code{return()} (which will
discard its value) and to signal an error use \code{stop()}.

The low-level interface to create a channel with custom behavior
is to call \code{channel(function(emit, reject, cancel) {...})},
providing your own function definition; your function will
receive those three callback methods as arguments. Then use
whatever means to arrange to call \code{emit(val)} some time in the
future as data comes in. When you are done emitting values, call
the \code{close()} callback. To report an error call
\code{reject(err)}; the next requestor will receive the error. If there
is more than one listener, other queued listeners will get a
\code{close} signal.
}
\author{
Peter Meilstrup
}
