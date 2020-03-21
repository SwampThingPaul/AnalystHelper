#' Helper function for axis labels using base plotting
#'
#' @param side an integer specifying which side of the plot the axis is to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and 4=right.
#' @param at major points at which tick-marks are to be drawn. Non-finite (infinite, NaN or NA) values are omitted.
#' @param at2 minor point at which tick-marks are to be drawn. Non-finite (infinite, NaN or NA) values are omitted.
#' @param labels major tick mark labels
#' @param cex.axis size of major axis labels text (default = 1)
#' @param line the number of lines into the margin at which the axis line will be drawn (default = -0.25)
#' @param lwd line widths for the axis line and the tick marks
#' @param maj.tcl length of major ticks
#' @param min.tcl length of minor ticks
#' @param las direction of text
#' @param axisLine the number of lines into the margin at which the axis line will be drawn
#' @keywords axis ticks
#' @return a function that provides major and minor ticks with labels
#' @note see axis() function
#' @examples
#' plot(runif(25),ylim=c(0,1),xlim=c(0,1),xaxt="n",yaxt="n")
#' axis_fun(1,seq(0,1,0.5),seq(0,1,0.25),seq(0,1,0.5))
#' axis_fun(2,seq(0,1,0.5),seq(0,1,0.25),seq(0,1,0.5))

axis_fun=function(side,at,at2,labels,cex.axis=1,line=-0.25,lwd=1,maj.tcl=-0.6,min.tcl=-0.3,las=1,axisLine=0){
  axis(side,line=line,at=at,labels=labels,las=las,tcl=maj.tcl,lty=0,cex.axis=cex.axis);
  axis(side,at=at,labels=F,las=las,tcl=maj.tcl,lwd=lwd,line=axisLine);
  axis(side,at=at2,labels=F,tcl=min.tcl,lwd=lwd,line=axisLine)
}
