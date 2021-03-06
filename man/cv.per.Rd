% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cv.per.R
\name{cv.per}
\alias{cv.per}
\title{Coefficient of variation}
\usage{
cv.per(data)
}
\arguments{
\item{data}{numeric}
}
\value{
calculated coefficient of variation (sd/mean)
}
\description{
Coefficient of variation
}
\examples{
set.seed(123)
value=rnorm(100)
cv.per(value)
}
