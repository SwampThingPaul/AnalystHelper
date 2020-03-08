#' Day of Water Year
#'
#'@param Date Date in as.POSIXct or as.Date format
#'@param WY.type Specifies Federal ("Fed") or Florida ("FL") water years
#'@keywords DOY DOWY
#'@export
#'@return Similar to "Day of the Year" calculations, this functions determines the Day of the Water Year.
#'@note See WY() function
#'@examples
#'dates=as.Date(c("2015-05-01","2015-08-20","2015-12-15"))
#'hydro.day(dates)

hydro.day = function(Date, WY.type="FL"){
  #require(lubridate)
  if(WY.type=="Fed"){start.month=10}
  if(WY.type=="FL"){start.month=5}
  Date=as.Date(Date)
  start.yr = year(Date) - (month(Date) < start.month)
  start.date = make_date(start.yr, start.month, 1L)
  DOWY=as.integer(Date - start.date + 1L)
  return(DOWY)
}


