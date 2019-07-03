#' Biweekly Period during Water Year
#'
#'@param Date Date in as.POSIXct or as.Date format
#'@param WY.type Specifies Federal ("Fed") or Florida ("FL") water years
#'@keywords DOWY biweek period
#'@export
#'@return Similar to "Week Number" function in base or lubridate, this function determines the two week period starting during any given water year (Federal or Florida).
#'@note See WY() function
#'@examples
#'dates=as.Date(c("2015-05-01","2015-08-20","2015-12-15"))
#'biweek.period(dates)

biweek.period=function(date,WY.type = "FL"){
  hydro.day(date)%/%14L+1L
}

