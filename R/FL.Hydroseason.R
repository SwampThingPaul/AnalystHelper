#' A function to return the wet/dry hydrologic season specific to south Florida
#'
#' @param Date Date in as.POSIXct or as.Date format
#' @keywords season date
#' @export
#' @return South Florida has a predictable wet and dry season due to the sub-tropical/tropical climate. Florida Department of Environmental Protection South Florida Water Management District and other agencies recognize this seasonality. May to October is considered the wet season and November to Apirl is the dry season for south Florida.
#' @examples
#' dates=as.Date(c("2015-05-01","2015-08-20","2015-12-15"))
#' FL.Hydroseason(dates)

FL.Hydroseason=function(Date){
  month=as.numeric(format(Date,"%m"))
  Hydroseason=ifelse(month%in%seq(5,10,1),"A_Wet","B_Dry")
  return(Hydroseason)
}
