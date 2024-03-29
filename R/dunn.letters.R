#' Plotting helper function to add letters indicating statistically similar parings.
#'
#' @param levels The number of groups
#' @param x a list of x variables
#' @param y a list of y variables
#' @param labels what text do you want to include
#' @param col2 color of the text label
#' @param cex2 size of label
#' @keywords Dunn comparison boxplot
#' @export
#' @return Helper function to add letters indicating statitcal significance.
#' @note Use in combination with dunn.test::dunn.test() and rcompanion::cldList()
#' @examples
#' \dontrun{
#' library(dunn.test)
#' library(rcompanion)
#'
#' set.seed(123)
#' data=data.frame(group=c(rep("alpha",5),rep("beta",5),rep("delta",5)),value=c(runif(5,max=2),runif(5,min=4,max=10),runif(5,min=12,max=30)))
#'
#' DT=with(data,dunn.test(value,group))
#' rslt=toupper(cldList(P.adjusted ~ comparison,data=DT,threshold = 0.05)$Letter)
#' boxplot(value~group,data,ylim=c(0,40))
#' dunn.letters(3,1:3,c(2,10,30),rslt,"red",1)
#' }

dunn.letters=function(levels,x,y,labels,col2=col2,cex2=cex2){
  .Deprecated('dunn.letters')
  # for(i in 1:levels){text(x=x[i],y=y[i],labels=paste0(labels[i]),col=col2,cex=cex2,pos=3)}
}
