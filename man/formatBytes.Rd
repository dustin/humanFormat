\name{formatBytes}
\alias{formatBytes}
\alias{formatSIBytes}
\alias{formatIECBytes}
\title{
Format a number of bytes into a human readable string.
}
\arguments{
  \item{b}{Number of bytes}
  \item{fmt}{String format for the numeric part of the bytes (defaults to \%.2f)}
}
\usage{
  formatBytes(b)
  formatSIBytes(b)
  formatIECBytes(b)

  formatBytes(b, fmt="\%.1f")
}
\description{
  Formats a number of bytes into a human readable string.

  When invoked as \code{formatBytes}, SI sizes are used.  You may
  specify IEC sizes by using \code{formatIECBytes}.

}
\examples{
  formatBytes(934818582)

  formatBytes(c(8429, 3525, 624624, 46, 7357))
}
