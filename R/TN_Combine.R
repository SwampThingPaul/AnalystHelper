#' Nitrogen concentration data handling
#'
#' @param NOx Nitrate-Nitrite (NOx)concentration (numeric)
#' @param TKN Total Kjeldahl Nitrogen (TKN) concentration (numeric)
#' @param TN Direct measure Total Nitrogen (TN) concentration(numeric)
#' @keywords "water quality" nitrogen
#' @export
#' @return This function handles and nitrogen data to calculate a common TN field
#' @examples
#' NOX=c(0.001,0.002,0.05,NA,NA)
#' TKN=c(0.5,0.05,0.4,NA,NA)
#' TN=c(NA,NA,NA,1.2,1.3)
#' TN_Combine(NOX,TKN,TN)
TN_Combine=function(NOx,TKN,TN){
  TN=ifelse(is.na(TN)==T,NA,TN)
  calc=ifelse(is.na(NOx)|is.na(TKN),NA,NOx+TKN)
  final=ifelse(is.na(TN),calc,TN)
  return(final)
}
SFWMD.TN.Combine=function(NOx,TKN,TN){
  #.Deprecated("TN.Combine")
  final=TN_Combine(NOx,TKN,TN)
  warning("This function is being phased out. Consider using TN_Combine in the future.")
  return(final)
}
