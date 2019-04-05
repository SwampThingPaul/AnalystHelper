#' converts Eastern Daylight Saving time (EDT) to Eastern Standard Time (EST)
#'
#' @param x Date (as.POSIXct)
#' @keywords date
#' @export
#' @return Helper function to convert EDT to EST date-time fields
#' @examples
#' datetime=as.POSIXct(c("2015-05-01 08:00:00","2015-10-15 12:00:00","2015-10-15 5:00:00"))
#' dst.to.est(datetime)

dst.to.est=function(x,to.tzone="EST"){
  require(lubridate)

  #dst.check=dst(x)
  attributes(x)$tzone=to.tzone
  return(x)
}
