% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cond.to.spc.R
\name{cond.to.spc}
\alias{cond.to.spc}
\title{Converts conductance to specific conductivity}
\usage{
cond.to.spc(temp, cond)
}
\arguments{
\item{SpCond}{specific conductance (microsiemens per centimeter)}

\item{Temp}{temperature (Degree C)}
}
\value{
Calculates specific conductivity consistent with Method 2510B of the Standard Methods for the Examination of Water and Wastewater (20th Edition).
}
\description{
Converts conductance to specific conductivity
}
\examples{
cond.to.spc(25.5,1321)
}
\keyword{Conductivity}
