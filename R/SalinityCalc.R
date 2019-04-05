#' Calculates salinity from Specific Conductance and Temperature data
#'
#' @param SpCond specific conductance (microsiemens per centimeter)
#' @param Temp temperature (Degree C)
#' @param Ref.Cond The standard reference conductance of seawater (42914 uS/cm)
#' @keywords Salinity
#' @export
#' @return Calculation based on UNESCO "Algorithms for computation of fundamental propoerties of seawater".
#' @examples
#' SalinityCalc(50512,25.5)

SalinityCalc=function(SpCond,Temp,Ref.Cond=42914){
  Cond=SpCond*(1+0.0191*(Temp-25))
  rt=0.6766097+0.0200564*Temp+0.0001104259*Temp^2+(-6.9698*10^-7)*Temp^3+(1.0031*10^-9)*Temp^4
  Rt=(Cond/Ref.Cond)/rt
  dS=((Temp-15)/(1+0.0162*(Temp-15)))*(0.0005+(-0.0056)*Rt^0.5+(-0.0066)*Rt+(-0.0375)*Rt^1.5+(0.0636)*Rt^2+(-0.0144)*Rt^2.5)
  Sal=0.008+(-0.1692)*Rt^0.5+25.3851*Rt+14.0941*Rt^1.5+(-7.0261)*Rt^2+2.7081*Rt^2.5+dS
  return(Sal)
}
