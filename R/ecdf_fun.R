#' eCDF with confidence intervals
#'
#' @param x numeric vector of the observations for ecdf; for the methods, an object inheriting from class "ecdf".
#' @param CI If TRUE CDF confidence intervals will be calculated; default is set to TRUE.
#' @param CI.interval Confidence interval; default is set to 95\%
#' @keywords cdf
#' @return Compute an empirical cumulative distribution function, returns a data frame with value and proporiton. Code based on base eCDF function.
#' @examples
#' set.seed(12)
#' test<-rnorm(100)
#' ecdf_fun(test)


ecdf_fun=function(x,CI=TRUE,CI.interval=0.95){
  #modifed from ecdf() function in stats.
  x <- sort(x)
  n <- length(x)
  vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n,
                    method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
  x.val=environment(rval)$x
  y.val=environment(rval)$y

  if(CI==TRUE){
    alpha=1-CI.interval
    eps=sqrt(log(2/alpha)/(2*n))
    ll=pmax(y.val-eps,0) 		# pmin and pmax do element wise min/max;  min and max would find the min/max of the entire vector
    uu=pmin(y.val+eps,1)
    return(data.frame(value=x.val,proportion=y.val,lwr.CI=ll,upr.CI=uu))
  }else{
    return(data.frame(value=x.val,proportion=y.val))
  }
}
