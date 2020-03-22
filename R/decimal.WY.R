#' Decimal water year from a date field
#'
#' @param date Date in as.POSIXct or as.Date format
#' @param WY.type Specifies Federal ("Fed") or Florida ("FL") water years
#' @keywords WY
#' @export
#' @return Returns decimal water year value based on Federal (Starts Oct 1) or Florida (Starts May 1),
#' @examples
#' # Decimal Water Year
#' decimal.WY(as.Date("2001-05-01"));

decimal.WY=function(date,WY.type="FL"){
  # calculates decimal water year (based on Florida WY)
  # similar to lubridate::decimal_dates()

  Y <- as.numeric(format(date,"%Y"))
  WY <- WY(date,WY.type=WY.type)
  tz.val<-if(is.null(attr(date,"tzone"))){Sys.timezone()}else{attr(date,"tzone")}

  if(WY.type=="FL"){
    start <- as.POSIXct(paste(WY-1, 5, 1,sep="-"), tz = tz.val)
    end <- as.POSIXct(paste(WY, 5, 1,sep="-"), tz =tz.val)
  }
  else if(WY.type=="Fed"){
    start <- as.POSIXct(paste(WY-1, 10, 1,sep="-"), tz = tz.val)
    end <- as.POSIXct(paste(WY, 10, 1,sep="-"), tz = tz.val)
  }else{NA}

  sofar <- as.numeric(difftime(date, start, units = "secs"))
  total <- as.numeric(difftime(end, start, units = "secs"))

  dec.dateWY <- WY + sofar/total
  return(dec.dateWY)
}
