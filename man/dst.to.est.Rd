% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dst.to.est.R
\name{dst.to.est}
\alias{dst.to.est}
\title{converts Eastern Daylight Saving time (EDT) to Eastern Standard Time (EST)}
\usage{
dst.to.est(x, to.tzone = "EST")
}
\arguments{
\item{x}{Date (as.POSIXct)}
}
\value{
Helper function to convert EDT to EST date-time fields
}
\description{
converts Eastern Daylight Saving time (EDT) to Eastern Standard Time (EST)
}
\examples{
datetime=as.POSIXct(c("2015-05-01 08:00:00","2015-10-15 12:00:00","2015-10-15 5:00:00"))
dst.to.est(datetime)
}
\keyword{date}
