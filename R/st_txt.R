#' Add text to map like raster::text()
#'
#' @param x sf object
#' @param labels character. Optional. Vector of labels with length(x) or a variable name from names(x)
#' @param halo 	logical. If TRUE a 'halo' is printed around the text. and hw=0.1 can be modified to set the colour and width of the halo
#' @param col character. Text color. If halo = TRUE, default is'black'
#' @param hc character. Halo color.  If halo = TRUE, default is 'white'
#' @param hw numeric. Halo width. If halo = TRUE, default is '0.1'
#' @param ... other arguments passed to text function
#'
#' @import sf
#' @import graphics
#'
#' @return spatial text label
#' @export

st_txt=function(x, labels, halo=T, col='black', hc='white', hw=0.1, ...){
  ## combination of code from raster::text(halo=T) and basf package

  if (missing(labels)) {
    labels <- 1
  }
  if (length(labels) != nrow(x)) {
    labels <- labels[1]
    if (is.character(labels)) {
      i <- which(labels == names(x))
      if (i == 0) {
        i <- 1
      }
    }
    labels <- x[[labels]]

  }
  xy <- sf::st_coordinates(sf::st_centroid(x))[,1:2, drop = FALSE]
  options(warn=-1)
  xy <- list(x = xy[,1, drop = TRUE], y = xy[,2, drop = TRUE])
  xo <- hw * graphics::strwidth('A')
  yo <- hw * graphics::strheight('A')

  if(halo==TRUE){
    theta <- seq(pi/4, 2*pi, length.out=8*hw*10)
    for (i in theta){
      text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=hc, ... )
    }
    text(xy$x, xy$y, labels, col=col, ... )}else{
      text(xy$x, xy$y, labels, col=col, ... )
    }

}

