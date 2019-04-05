#' Converts conductance to specific conductivity
#'
#' @param SpCond specific conductance (microsiemens per centimeter)
#' @param Temp temperature (Degree C)
#' @keywords Conductivity
#' @export
#' @return Calculates specific conductivity consistent with Method 2510B of the Standard Methods for the Examination of Water and Wastewater (20th Edition).
#' @examples
#' cond.to.spc(25.5,1321)

cond.to.spc=function(temp,cond){
  spc=cond/(1+0.0191*(temp-25))
  return(spc)
}
