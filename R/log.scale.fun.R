#' Generate Log-base 10 sequence of values to be used in conjunction with axis_fun when plotting log data
#'
#' @param val range of values
#' @param type major or minor
#' @keywords axis ticks log-scale
#' @return a function that provides major and minor ticks with labels on a log scale
#' @note see axis() and axis_fun() function
#' @export log.scale.fun
#' @examples
#' \dontrun{
#' # set.seed(36)
#' # x.val=exp(rlnorm(100))
#' # y.val=exp(rlnorm(100))
#'
#' # xlim.val=c(1,100)
#' # ylim.val=c(1,5000)
#' # plot(y.val~x.val,ylim=ylim.val,xlim=xlim.val,log="xy",yaxt="n",xaxt="n")
#' # axis_fun(1,log.scale.fun(xlim.val,"major"),log.scale.fun(xlim.val,"minor"),log.scale.fun(xlim.val,"major"))
#' # axis_fun(2,log.scale.fun(ylim.val,"major"),log.scale.fun(ylim.val,"minor"),log.scale.fun(ylim.val,"major"))
#' }


log.scale.fun=function(val,type){
  min.val=floor(log10(min(val)))
  max.val=ceiling(log10(max(val)))
  maj.tck=10^seq(min.val,max.val,1)
  min.tck=c()
  for(i in 2:length(maj.tck)){
    tmp=seq(maj.tck[i-1],maj.tck[i],maj.tck[i-1])
    min.tck=c(min.tck,tmp)
  }
  return(switch(type,
                minor = min.tck,
                major = maj.tck,
                stop("type argument must be 'major' or 'minor'")
  ))
}
