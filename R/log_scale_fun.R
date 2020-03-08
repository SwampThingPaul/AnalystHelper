#' Generate Log-base 10 sequence of values to be used in conjunction with axis_fun when plotting log data
#'
#' @param val range of values
#' @param type major or minor
#' @keywords axis ticks log-scale
#' @export
#' @return a function that provides major and minor ticks with labels on a log scale
#' @note see axis() and axis_fun() function
#' @examples
#' set.seed(36)
#' x.val=exp(rlnorm(100))
#' y.val=exp(rlnorm(100))
#'
#' xlim.val=c(1,100)
#' ylim.val=c(1,5000)
#' plot(y.val~x.val,ylim=ylim.val,xlim=xlim.val,log="xy",yaxt="n",xaxt="n")
#' xmaj=log_scale_fun(xlim.val,"major")
#' xmin=log_scale_fun(xlim.val,"minor")
#' ymaj=log_scale_fun(ylim.val,"major")
#' ymin=log_scale_fun(ylim.val,"minor")
#'
#' axis_fun(1,xmaj,xmin,xmaj)
#' axis_fun(2,yamj,ymin,ymaj)


log_scale_fun=function(val,type){
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
