\name{formatBytes}
\alias{formatBytes}
\alias{formatSIBytes}
\alias{formatIECBytes}
\title{
Format a number of bytes into a human readable string.
}
\arguments{
  \item{b}{Number of bytes}
  \item{fmt}{String format for the numeric part of the bytes}
}
\usage{
  formatBytes(b, fmt="\%.2f")
  formatSIBytes(b, fmt="\%.2f")
  formatIECBytes(b, fmt="\%.2f")
}
\description{
  Formats a number of bytes into a human readable string.

  When invoked as \code{formatBytes}, SI sizes are used.  You may
  specify IEC sizes by using \code{formatIECBytes}.
}
\examples{
  # returns "934.82 MB"
  formatBytes(934818582)

  # returns "891.51 MiB"
  formatIECBytes(934818582)

  # returns c("8.43 KB", "3.52 KB", "624.62 KB", "46", "7.36 KB")
  formatBytes(c(8429, 3525, 624624, 46, 7357))
}
