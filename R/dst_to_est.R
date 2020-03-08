#' converts Eastern Daylight Saving time (EDT) to Eastern Standard Time (EST)
#'
#' @param x Date (as.POSIXct)
#' @keywords date
#' @export
#' @return Helper function to convert EDT to EST date-time fields
#' @examples
#' datetime=as.POSIXct(c("2015-05-01 08:00:00","2015-10-15 12:00:00","2015-10-15 5:00:00"))
#' dst_to_est(datetime)

<<<<<<< HEAD:R/dst_to_est.R
dst_to_est=function(x,to.tzone="EST"){
  #require(lubridate)
=======
dst.to.est=function(x,to.tzone="EST"){
  require(lubridate)
>>>>>>> parent of 455f307... update and edits:R/dst.to.est.R

  dst.check=dst(x)
  attributes(x)$tzone=to.tzone
  return(x)
}
