#' Random conversion helpers
#' @name conversions
#' @param x numeric value
#' @aliases conversions
#' @details converts data from one unit to another.
#' \itemize{
#' \item{cfs.to.m3d}
#' \item{cfs.to.km3d}
#' \item{cfs.to.acftd}
#' \item{mt.to.kg}
#' \item{kg.to.mt}
#' \item{acres.to.m2}
#' \item{m3d.to.cfs}
#' \item{ft.to.m}
#' \item{m.to.ft}
#' \item{degF.to.C}
#' \item{in.to.cm}
#' \item{hPa.to.mmHg}
#' }
#' @export
#' @examples
#' cfs.to.m3d(350)

cfs.to.m3d=function(x) x*2446.58
cfs.to.km3d=function(x) x*2.44658e-6
cfs.to.acftd=function(x) x*1.98347
mt.to.kg=function(x) x*1000
kg.to.mt=function(x) x/1000
acres.to.m2=function(x)x*4046.86
m3d.to.cfs=function(x) x/2446.58
ft.to.m=function(x) x*0.3048
m.to.ft=function(x) x*3.28084
degF.to.C=function(x) x-32*(5/9)
in.to.cm=function(x) x*2.54
hPa.to.mmHg=function(x) x*0.75006157584566
