#' windspeed and direction helper functions for base plotting
#'
#' @param DateTime Date and time field as.POSIXct
#' @param WSPD Wind speed in m/s
#' @param WD Wind direction in degrees
#' @param data data.frame()
#' @keywords wind base-plot
#' @export
#' @return draws line segments in the direct of wind with length corresponding to speed.
#' @examples
#' wx.dat2005=read.table("https://www.ndbc.noaa.gov/view_text_file.php?filename=lonf1h2005.txt.gz&dir=data/historical/stdmet/",sep="",header=T,na.strings=c("99","999","9999.0"))
#'
#' plot(WSPD~DateTime.EST,wx.dat2005.tmp,type="n",ylab="Wind Speed (m/s)",ylim=c(-20,30))
#' stickplot.dat.arrows(DateTime.EST,WSPD,WD,wx.dat2005.tmp,col="green",lty=1,lwd=1)

stickplot.dat.arrows=function(DateTime,WSPD,WD,data,...){
  require(lubridate)

  if(!missing(data)){
    DateTime=data[,deparse(substitute(DateTime))]
    WSPD=data[,deparse(substitute(WSPD))]
    WD=data[,deparse(substitute(WD))]
  }

  y=rep(0,length(DateTime))
  xend=DateTime + lubridate::dhours(WSPD * 1 * -cos((90-WD) / 360 * 2 * pi))
  yend = WSPD * 1 * -sin((90-WD) / 360 * 2 * pi)
  tmp=data.frame(DateTime=DateTime,y=y,xend=xend,yend=yend)
  arrows(DateTime,y,xend,yend,length=0,...)
}
