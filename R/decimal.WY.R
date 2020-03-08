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

  Y <- year(date)
  WY <- WY(date,WY.type=WY.type)

  if(WY.type=="FL"){
    start <- make_datetime(WY-1, 5L, 1L, tz = tz(date))
    end <- make_datetime(WY, 5L, 1L, tz = tz(date))
  }
  else if(WY.type=="Fed"){
    start <- make_datetime(WY-1, 10L, 1L, tz = tz(date))
    end <- make_datetime(WY, 10L, 1L, tz = tz(date))
  }else{NA}

  sofar <- as.numeric(difftime(date, start, units = "secs"))
  total <- as.numeric(difftime(end, start, units = "secs"))

  dec.dateWY <- WY + sofar/total
  return(dec.dateWY)
}
