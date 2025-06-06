% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uid.r
\name{uid}
\alias{uid}
\alias{hash}
\title{Generate Names}
\usage{
uid()

hash(str)
}
\arguments{
\item{str}{A string (scalar character).}
}
\value{
A string (scalar character) that can be used as a mutex,
semaphore, or message queue name.
}
\description{
To ensure broad compatibility across different operating systems, names of
mutexes, semaphores, and message queues should start with a letter followed
by up to 249 alphanumeric characters. These functions generate names meeting
these requirements.\cr\cr
\itemize{
\item \code{uid()}: 11-character encoding of PID and time since epoch.
\item \code{hash()}: 11-character hash of any string (hash space = 2^64).
}
}
\details{
\code{uid()}s encode sequential 1/100 second intervals, beginning at the current
process's start time. If the number of requested UIDs exceeds the number of
1/100 seconds that the process has been alive, then the process will
momentarily sleep before returning.

A \code{uid()} begins with an uppercase letter (\code{A - R}); a \code{hash()} begins with
a lowercase letter (\code{a - v}).
}
\examples{

    library(interprocess)
    
    uid()
    
    hash('192.168.1.123:8011')
    
}
