#' Returns sample size
#'
#' @param x numeric value
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @keywords count
#' @export
#' @return return the number of real values.
#' @examples
#'example=c(rnorm(12),NA,NA,NA)
#'# N(example);# versus length(example)
#'N.obs(example)

#N=function(x,NA.val="NA") length(which(x!=NA.val))
#NA.val the value to exclude, default is 'NA'

N=function(x,na.rm=FALSE){
.Deprecated("N.obs")
}


#' @export
N.obs=function(x,na.rm=FALSE){
  ind <- is.na(x) | is.nan(x) | is.infinite(x)
  return(length(x[!ind]))
}
