#' Date function to convert character/other as.POSIXct to date.
#'
#' @param x as.POSIXct or as.character
#' @param tz timezone (default set to Eastern Standard Time)
#' @param form format (default set to YYYY-mm-dd)
#' @keywords date
#' @export
#' @return Helper function to format date (and date-time) fields
#' @examples
#' datetime=as.character(c("2015-05-01 08:00:00","2015-10-15 12:00:00","2015-10-15 5:00:00"))
#' date_fun(datetime);# just date
#' date_fun(datetime, form="%F %X"); #include date and time

date_fun=function(x,tz="EST",form="%F"){
  as.POSIXct(strptime(x,form),tz=tz)
}
