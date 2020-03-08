#' Nutrient Load: Linearly interpolates data to conduct nutrient load calculations
#'
#' @param x numeric
#' @keywords Nutrient load
#' @export
#' @return one of two functions used to calculate daily nutrient load from flow and water quality parameters
#' @examples
#' data=c(rep(NA,5),runif(2,2,5),NA,NA,NA,6,7,8, NA,NA)
#' dat.interp(data)

dat.interp=function(x){
  require(zoo)
  val=na.approx(x,na.rm=F)
  val=na.locf(val,na.rm=F,fromLast=F)
  val=na.locf(val,na.rm=F,fromLast=T)
  return(val)
}
