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
#' ## Function id deprecated


dunn.letters=function(levels,x,y,labels,col2=col2,cex2=cex2){
  .Deprecated('dunn.letters')
  # for(i in 1:levels){text(x=x[i],y=y[i],labels=paste0(labels[i]),col=col2,cex=cex2,pos=3)}
}
