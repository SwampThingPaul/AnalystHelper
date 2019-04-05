#' Point with error bars
#'
#' @param x x-value
#' @param y y-value (or mean)
#' @param SE standard error
#' @param ln.lty line type
#' @param ln.col color of lines
#' @param ln.lwd width of lines
#' @param pch plotting ‘character’, i.e., symbol to use. see points()
#' @param bg background (fill) color for the open plot symbols given by pch = 21:25. see points()
#' @param cex size of point
#' @param pt.lwd line width of points
#' @param pt.lty line type for points
#' @param er.lwd error bar line width
#' @param pt.col color of point
#' @param length length of the edges of the error bar head (in inches).
#' @keywords error
#' @export
#' @return draws lines in the form of error bars
#' @note  see arrows(), errorbars() and pt_error() functions
#' @examples
#'x.val=c(1,2,3)
#'mean.val=c(2,5,7)
#'se.val=c(0.1,0.5,1)
#'plot(mean.val~x.val,ylim=c(0,10),type="n")
#'pt_line_error(x.val,mean.val,se.val,2,"grey",1,21,"dodgerblue1")

pt_line_error=function(x,y,SE,ln.lty,ln.col,ln.lwd,pch,bg,cex=1,pt.lwd=1,pt.lty=1,er.lwd=1,pt.col="black",length=0.07){
  lower=ifelse(is.na(y)==T|is.na(SE)==T,NA,y-SE)
  upper=ifelse(is.na(y)==T|is.na(SE)==T,NA,y+SE)
  lines(x,y,lty=ln.lty,col=ln.col,lwd=ln.lwd)
  arrows(x,lower,x,upper,col=ln.col,lty=1,length=length,angle=90,code=3,lwd=er.lwd);
  points(x,y,pch=pch,lty=pt.lty,col=pt.col,lwd=pt.lwd/2,bg=bg,cex=cex)
  options(warn=-1)
}
