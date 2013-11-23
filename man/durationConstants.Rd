\name{durationConstants}
\alias{kNanosecond}
\alias{kMicrosecond}
\alias{kMillisecond}
\alias{kSecond}
\alias{kMinute}
\alias{kHour}
\title{
Duration constants
}
\details{
  These are useful for computing specific time durations or for
  converting from quantities in one unit to another unit
  (e.g. time in milliseconds to time in nanoseconds for duration
  formatting).

  \itemize{
    \item \code{kNanosecond}: One nanosecond;
    \item \code{kMicrosecond}: One microsecond (1000 nanosecond);
    \item \code{kMillisecond}: One millisecond (1000 microseconds);
    \item \code{kSecond}: One second (1000 milliseconds);
    \item \code{kMinute}: One minute (60 seconds);
    \item \code{kHour}: One hour (60 minutes).
  }
}
\usage{
kNanosecond
kMicrosecond
kMillisecond
kSecond
kMinute
kHour
}
\description{
  These constants are used to express the number of nanoseconds
  in different time units.
}
\examples{
  # To calculate 4 minute, 3 seconds and 14 microseconds:
  4*kMinute + 3*kSecond + 14*kMicrosecond
}
