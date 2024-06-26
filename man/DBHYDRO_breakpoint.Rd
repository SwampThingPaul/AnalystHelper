% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBHYDRO_breakpoint.R
\name{DBHYDRO_breakpoint}
\alias{DBHYDRO_breakpoint}
\title{Data retrieval from South Florida Water Management District online database (DBHYDRO)}
\usage{
DBHYDRO_breakpoint(
  SDATE,
  EDATE,
  DBK,
  vert_datum = NA,
  dataonly = TRUE,
  period = "uspec",
  v_target_code = "file_csv",
  ...
)
}
\arguments{
\item{SDATE}{input start date as.Date (YYYY-MM-DD)}

\item{EDATE}{input end date as.Date (YYYY-MM-DD)}

\item{DBK}{SFWMD daily data site and data type identifier}

\item{dataonly}{DEFAULT is TRUE to return a data.frame of data, if set to FALSE it will return a nested list of two data.frame labeled METADATA and REPORT.}

\item{period}{DEFAULT set to "uspec" for URL query}

\item{v_target_code}{DEFAULT set to "file_csv" to return a CSV file}

\item{...}{to allow for more functionality and flexibility in building URL queries}
}
\value{
This function returns breakpoint (i.e. 15-minute) hydrometerological (discharge, stage(WL) and meterological parameters) dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management.
}
\description{
Data retrieval from South Florida Water Management District online database (DBHYDRO)
}
\examples{
\dontrun{
# Daily Discharge Data
sdate=as.Date("2001-05-01");
edate=as.Date("2001-06-01");
dat=DBHYDRO_breakpoint(SDATE,EDATE,"AI516")
}
}
\keyword{discharge}
\keyword{stage}
\keyword{weather}
