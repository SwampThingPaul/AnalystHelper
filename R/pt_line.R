#' Base plotting helper function.
#'
#' @param x x-value
#' @param y y-value
#' @param ln.lty line type
#' @param ln.col  line color
#' @param ln.lwd line width
#' @param pch plotting ‘character’, i.e., symbol to use. see points()
#' @param bg background (fill) color for the open plot symbols given by pch = 21:25. see points()
#' @param cex point size
#' @param pt.lwd line width of points
#' @param pt.lty line type for points
#' @param pt.col  color of point
#' @keywords points lines
#' @export
#' @return draws points and lines together.
#' @note  see points() and lines() functions
#' @examples
#'x.val=c(1,2,3)
#'mean.val=c(2,5,7)
#'se.val=c(0.1,0.5,1)
#'plot(mean.val~x.val,ylim=c(0,10),type="n")
#'pt_line(x.val,mean.val,2,"grey",1,21,"dodgerblue1")

pt_line=function(x,y,ln.lty,ln.col,ln.lwd,pch,bg,cex=1,pt.lwd=0.25,pt.lty=1,pt.col="black"){
  lines(x,y,lty=ln.lty,col=ln.col,lwd=ln.lwd)
  points(x,y,pch=pch,lty=pt.lty,col=pt.col,lwd=pt.lwd,bg=bg,cex=cex)
}
