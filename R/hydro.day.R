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
  if(WY.type=="Fed"){start.month=10}
  if(WY.type=="FL"){start.month=5}
  Date=as.Date(Date)
  tz.val<-if(is.null(attr(Date,"tzone"))){Sys.timezone()}else{attr(Date,"tzone")}
  start.yr = as.numeric(format(Date,"%Y")) - (as.numeric(format(Date,"%m")) < start.month)
  start.date =as.Date(paste(start.yr,start.month,1,sep="-"),tz=tz.val)
  DOWY=as.integer(Date - as.Date(start.date) + 1L)
  return(DOWY)
}



