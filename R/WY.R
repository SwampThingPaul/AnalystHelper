#' Determines Water Year from a date field
#'
#' @param date Date in as.POSIXct or as.Date format
#' @param WY.type Specifies Federal ("Fed") or Florida ("FL") water years
#' @keywords WY
#' @export
#' @return Returns water year value based on Federal (Starts Oct 1) or Florida (Starts May 1),
#' @examples
#' dates=as.Date(c("2015-05-01","2015-08-20","2015-12-15"))
#' WY(dates)

WY=function(date,WY.type="FL"){
  if(WY.type=="FL"){ifelse(as.numeric(format(date,"%m"))>4,as.numeric(format(date,"%Y"))+1,as.numeric(format(date,"%Y")))}
  else if(WY.type=="Fed"){ifelse(as.numeric(format(date,"%m"))>9,as.numeric(format(date,"%Y"))+1,as.numeric(format(date,"%Y")))}else{NA}
}
