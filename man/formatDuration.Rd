\name{formatDuration}
\alias{formatDuration}
\alias{formatNanoseconds}
\alias{formatMicroseconds}
\alias{formatMilliseconds}
\alias{formatSeconds}
\title{
Format nanosecond durations into human readable strings.
}
\arguments{
  \item{ns}{Duration in nanoseconds}
  \item{us}{Duration in microseconds}
  \item{ms}{Duration in milliseconds}
  \item{s}{Duration in seconds}
}
\usage{
  formatDuration(ns)
  formatNanoseconds(ns)

  formatMicroseconds(us)
  formatMilliseconds(ms)
  formatSeconds(s)
}
\description{
  Format individual or vectors of durations into human friendly text.
}
\seealso{
  \code{\link{durationConstants}} constants for various duration lengths
}
\examples{
  formatDuration(0)
  formatDuration(1)
  formatNanoseconds(34)

  formatMicroseconds(235)

  formatMilliseconds(2487)

  formatSeconds(7213)

  formatDuration(c(0, 1, 1000, 2039, 205958, 284859249525))
}
