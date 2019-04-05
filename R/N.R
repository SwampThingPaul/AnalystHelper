#' Returns sample size
#'
#' @param x numeric value
#' @param NA.val the value to exclude, default is 'NA'
#' @keywords count
#' @export
#' @return return the number of real values.
#' @examples
#'example=c(rnorm(12),NA,NA,NA)
#'N(example);# versus length(example)

N=function(x,NA.val="NA") length(which(x!=NA.val))
