\name{formatDuration}
\alias{formatDuration}
\title{
Format nanosecond durations into human readable strings.
}
\arguments{
  \item{ns}{Duration in nanoseconds}
}
\usage{
  formatDuration(ns)
}
\description{
  Format individual or vectors of nanosecond durations into
  human friendly text.
}
\examples{
  formatDuration(0)
  formatDuration(1)

  formatDuration(c(0, 1, 1000, 2039, 205958, 284859249525))
}
