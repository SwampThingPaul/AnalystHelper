#' converts Eastern Daylight Saving time (EDT) to Eastern Standard Time (EST)
#'
#' @param x Date (as.POSIXct)
#' @keywords date
#' @return Helper function to convert EDT to EST date-time fields
#' @export dst.to.est
#' @examples
#' datetime=as.POSIXct(c("2015-05-01 08:00:00","2015-10-15 12:00:00","2015-10-15 5:00:00"))
#' dst.to.est(datetime)

dst.to.est=function(x,to.tzone="EST"){
  #dst.check=c(NA, FALSE, TRUE)[as.POSIXlt(x)$isdst + 2]
  attributes(x)$tzone=to.tzone
  return(x)
}
