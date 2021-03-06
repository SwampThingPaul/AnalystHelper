% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TN_Combine.R
\name{TN_Combine}
\alias{TN_Combine}
\title{Nitrogen concentration data handling}
\usage{
TN_Combine(NOx, TKN, TN)
}
\arguments{
\item{NOx}{Nitrate-Nitrite (NOx)concentration (numeric)}

\item{TKN}{Total Kjeldahl Nitrogen (TKN) concentration (numeric)}

\item{TN}{Direct measure Total Nitrogen (TN) concentration(numeric)}
}
\value{
This function handles and nitrogen data to calculate a common TN field
}
\description{
Nitrogen concentration data handling
}
\examples{
NOX=c(0.001,0.002,0.05,NA,NA)
TKN=c(0.5,0.05,0.4,NA,NA)
TN=c(NA,NA,NA,1.2,1.3)
TN_Combine(NOX,TKN,TN)
}
\keyword{"water}
\keyword{nitrogen}
\keyword{quality"}
