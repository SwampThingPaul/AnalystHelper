% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_txt.R
\name{st_txt}
\alias{st_txt}
\title{Add text to map like raster::text()}
\usage{
st_txt(x, labels, halo = T, col = "black", hc = "white", hw = 0.1, ...)
}
\arguments{
\item{x}{sf object}

\item{labels}{character. Optional. Vector of labels with length(x) or a variable name from names(x)}

\item{halo}{logical. If TRUE a 'halo' is printed around the text. and hw=0.1 can be modified to set the colour and width of the halo}

\item{col}{character. Text color. If halo = TRUE, default is'black'}

\item{hc}{character. Halo color.  If halo = TRUE, default is 'white'}

\item{hw}{numeric. Halo width. If halo = TRUE, default is '0.1'}

\item{...}{other arguments passed to text function}
}
\value{
spatial text label
}
\description{
Add text to map like raster::text()
}
