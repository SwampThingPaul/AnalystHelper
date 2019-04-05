#' Point with error bars
#'
#' @param x x-value
#' @param y y-value (or mean)
#' @param SE standard error
#' @param pch plotting ‘character’, i.e., symbol to use. see points()
#' @param bg background (fill) color for the open plot symbols given by pch = 21:25. see points()
#' @param ln.col color of error bar lines
#' @param ln.lwd width of error bar lines
#' @param cex size of point
#' @param pt.lwd line width of points
#' @param pt.lty line type for points
#' @param pt.col color of point
#' @param length length of the edges of the error bar head (in inches).
#' @keywords error
#' @export
#' @return draws lines in the form of error bars
#' @note  see arrows() and errorbars() functions
#' @examples
#'x.val=c(1,2,3)
#'mean.val=c(2,5,7)
#'se.val=c(0.1,0.5,1)
#'plot(mean.val~x.val,ylim=c(0,10),type="n")
#'pt_error(x.val,mean.val,se.val,21,"dodgerblue1","grey",2,cex=2)


pt_error=function(x,y,SE,pch,bg,ln.col,ln.lwd,cex=1,pt.lwd=0.5,pt.lty=1,pt.col="black",length=0.07){
  lower=y-SE
  upper=y+SE
  arrows(x,lower,x,upper,col=ln.col,lty=1,lwd=ln.lwd,length=length,angle=90,code=3);
  points(x,y,pch=pch,col=pt.col,lwd=pt.lwd,bg=bg,cex=cex)
}
