#' Calculation of theortical quantile (Quantile-Quantile Plot)
#'
#' @param y data sample
#' @export
#' @return Part of the qqnorm generic function
#' @examples
#' set.seed(123)
#' x.val=rnorm(100)
#' y.val=x.val+100
#' test=lm(y.val~x.val)
#' qq.function(test$residuals)

qq.function=function(y){
  n=length(y)
  r=order(order(y))
  if(n>10){p=(r-1/2)/n}else{p=(r-3/8)/(n+1/4)}
  qqnorm.val=qnorm(p)
  return(qqnorm.val)
}
