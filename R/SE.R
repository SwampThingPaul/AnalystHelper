#' Calcualtes standard error
#'
#' @param x numeric value
#' @param NA.val the value to exclude, default is 'NA'
#' @keywords error
#' @export
#' @return Calculates standard error (sd()/sqrt(n)).
#' @examples
#' set.seed(123)
#' example=c(rnorm(12),NA,NA,NA)
#' SE(example);#should return 0.2671171
#'

SE=function(x,NA.val="NA") sd(x,na.rm=T)/sqrt(length(which(x!=NA.val)))
