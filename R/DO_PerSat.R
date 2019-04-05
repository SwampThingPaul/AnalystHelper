#' Calculates dissolved oxygen percent saturation from concentration.
#'
#' @param Temp temperature (Degree C)
#' @param DO Dissolved oxygen (miligrams per liter)
#' @param sal Salinity
#' @keywords Oxygen
#' @export
#' @return  DO_PerSat() calculated dissolved oxygen percent saturation consistent with Florida Department of Environmental Protection technical support documents (https://floridadep.gov/sites/default/files/tsd-do-criteria-aquatic-life.pdf) using water temperature, dissolved oxygen concentration and salinity.
#' @examples
#' DO_PerSat(25.5,15.1,0); #Freshwater
#' DO_PerSat(25.5,15.1,33); #Marine

DO_PerSat=function(Temp,DO,sal){
  tempK=Temp+273.15
  DO.PerSat=round(DO/(exp((-139.34411+(157570.1/tempK)-(66423080/tempK^2)+(12438000000/tempK^3)-(862194900000/tempK^4))-(sal*(0.017674-(10.754/tempK)+(2140.7/tempK^2)))))*100,1)
  return(DO.PerSat)
}
