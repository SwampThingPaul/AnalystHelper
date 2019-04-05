#' Nutrient Load: Calculates load from discharge and concentration data
#'
#' @param flow.cfs Daily discharge data in cubic feet per second
#' @param conc.mgL Nutrient (or other parameter) concentration data in milligrams per liter
#' @keywords nutrient load
#' @note see dat.interp()
#' @return Calculates load in kilograms from discharge (cfs) and concentration (mg/L)
#' @examples
#' Load.Calc.kg(350,0.0012);#350 cfs and 0.0012 mg/L

Load.Calc.kg=function(flow.cfs,conc.mgL){
  q.cmd=flow.cfs*2446.57555
  conc.mgm3=conc.mgL*1000
  load.kg=(q.cmd*conc.mgm3)*1e-6
  return(load.kg)
}
