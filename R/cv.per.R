#' Coefficient of variation
#'
#' @param data numeric
#' @export
#' @return calculated coefficient of variation (sd/mean)
#' @examples
#' set.seed(123)
#' value=rnorm(100)
#' cv.per(value)

cv.per=function(data){(sd(data,na.rm=T)/mean(data,na.rm=T))}
