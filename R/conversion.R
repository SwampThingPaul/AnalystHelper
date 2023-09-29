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
#' @examples
#' cfs.to.m3d(350)

#' @export
cfs.to.m3d=function(x) x*2446.57562905453
#' @export
cfs.to.km3d=function(x) x/408700
#' @export
cfs.to.acftd=function(x) x*1.98347114207859
#' @export
mt.to.kg=function(x) x*1000
#' @export
kg.to.mt=function(x) x/1000
#' @export
acres.to.m2=function(x)x*4046.86
#' @export
m3d.to.cfs=function(x) x/2446.57562905453
#' @export
ft.to.m=function(x) x*0.3048
#' @export
m.to.ft=function(x) x*3.28084
#' @export
degF.to.C=function(x) (x-32)*(5/9)
#' @export
degC.to.F=function(x) (x*9/5)+32
#' @export
in.to.cm=function(x) x*2.54
#' @export
hPa.to.mmHg=function(x) x*0.75006157584566
#' @export
acft.to.L=function(x) x*1233481.8375475
#' @export
acft.to.m3d=function(x) x*1233.4818375475
#' @export
m2.to.acres=function(x) x/4046.86
