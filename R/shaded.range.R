#' Base plotting helper function to draw a shaded polygon
#'
#' @param x x-value
#' @param y.L lower y-value
#' @param y.U upper y-value
#' @param bg background shading
#' @param col line color
#' @param lty line type
#' @param col.adj factor modifying the opacity alpha; typically in [0,1] see alpha.f in adjustcolor()
#' @param lwd lines width around polygon
#' @keywords shaded
#' @export
#' @return draws polygon based on x, y (upper and lower) values
#' @note  see polygon() function
#' @examples
#' set.seed(456)
#' y.val=rnorm(100)
#' x.val=y.val*rnorm(100)
#' mod=lm(y.val~x.val)
#' x.pred=seq(min(x.val),max(x.val),max(x.val)/10)
#' mod.pred=predict(mod, newdata=data.frame(x.val=x.pred),interval="confidence")
#'
#' plot(y.val~x.val,type="n")
#' shaded.range(x.pred,mod.pred[,2],mod.pred[,3],"grey")
#' points(x.val,y.val,pch=19)

shaded.range=function(x,y.L,y.U,bg,col=bg,lty=3,col.adj=0.25,lwd=1){
  xx=c(x,rev(x))
  yy=c(y.L,rev(y.U))
  polygon(xx,yy,col=adjustcolor(bg,col.adj),border=col,lty=lty,lwd=lwd)
}
