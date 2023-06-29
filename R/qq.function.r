#' Calculation of theortical quantile (Quantile-Quantile Plot)
#'
#' @param y data sample
#' @export
#' @importFrom stats approxfun qnorm sd
#' @return Part of the qqnorm generic function
#' @examples
#' # set.seed(123)
#' # test=lm(rnorm(100)~rnorm(100)+100)
#' # qq.function(test$residuals)


qq.function=function(y){
  n=length(y)
  r=order(order(y))
  if(n>10){p=(r-1/2)/n}else{p=(r-3/8)/(n+1/4)}
  qqnorm.val=qnorm(p)
  return(qqnorm.val)
}
