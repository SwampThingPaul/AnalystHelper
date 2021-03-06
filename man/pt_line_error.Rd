% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pt_line_error.R
\name{pt_line_error}
\alias{pt_line_error}
\title{Point with error bars}
\usage{
pt_line_error(
  x,
  y,
  SE,
  ln.lty,
  ln.col,
  ln.lwd,
  pch,
  bg,
  cex = 1,
  pt.lwd = 1,
  pt.lty = 1,
  er.lwd = 1,
  pt.col = "black",
  length = 0.07
)
}
\arguments{
\item{x}{x-value}

\item{y}{y-value (or mean)}

\item{SE}{standard error}

\item{ln.lty}{line type}

\item{ln.col}{color of lines}

\item{ln.lwd}{width of lines}

\item{pch}{plotting ‘character’, i.e., symbol to use. see points()}

\item{bg}{background (fill) color for the open plot symbols given by pch = 21:25. see points()}

\item{cex}{size of point}

\item{pt.lwd}{line width of points}

\item{pt.lty}{line type for points}

\item{er.lwd}{error bar line width}

\item{pt.col}{color of point}

\item{length}{length of the edges of the error bar head (in inches).}
}
\value{
draws lines in the form of error bars
}
\description{
Point with error bars
}
\note{
see arrows(), errorbars() and pt_error() functions
}
\examples{
x.val=c(1,2,3)
mean.val=c(2,5,7)
se.val=c(0.1,0.5,1)
plot(mean.val~x.val,ylim=c(0,10),type="n")
pt_line_error(x.val,mean.val,se.val,2,"grey",1,21,"dodgerblue1")
}
\keyword{error}
