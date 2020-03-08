#' Florida Department of Environmental Protection Time-of-day dissolved oxygen water quality criteria
#'
#' @param DateTime Date and Time field in as.POSIXct() format
#' @keywords Oxygen
#' @export
#' @return  DO.TOD.WQS.stream() calculates the time-of-day dissolved oxygen water quality criteria consistent with 62-302.533 Florida Adminstrative Code (FAC) for stream ecosystems. Development of this method can be found at https://floridadep.gov/sites/default/files/tsd-do-criteria-aquatic-life.pdf
#' @examples
#' Date.Time=as.POSIXct(c("2015-05-01 08:00","2015-10-15 12:00"))
#' DO.TOD.WQS.stream(Date.Time)

DO.TOD.WQS.stream=function(DateTime){
  Minutes=as.numeric((hour(DateTime)*60+minute(DateTime)))
  DO_TOD=round(0.00000000000019888*(Minutes^5)-0.00000000068941*(Minutes^4)+0.00000078373*(Minutes^3)-0.00031598*(Minutes^2)+0.03551*Minutes+33.43,1)
  return(DO_TOD)
}
