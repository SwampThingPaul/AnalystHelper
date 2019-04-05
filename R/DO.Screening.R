#' Function to screen dissolved oxygen data within the Everglades Ecosystem prior to evaluating compliance with established water quality standards.
#'
#' @param data data.frame() with DateTime, Dissolved Oxygen (concentration) and Temperature data
#' @keywords Oxygen
#' @export
#' @return   DO.Screening() assumes the data has a "DateTime.EST" field (date and time stamp), "DO" Field and "Temp" field to determine suitable data. No data is removed just a "UseData" field is added.
#' @examples
#' data=data.frame(DateTime.EST=as.POSIXct(c("2015-05-01 08:00","2015-10-15 12:00","2015-10-15 5:00")),DO=c(35,2,3),Temp=c(25,27,3))
#' DO.Screening(data)

DO.Screening=function(data){
  data$TimeFlag=with(data,ifelse(as.numeric(format(DateTime.EST,"%H"))==0,1,ifelse(as.numeric(format(DateTime.EST,"%H"))<6&as.numeric(format(DateTime.EST,"%H"))>18,1,0)))
  data$DOFlag=with(data,ifelse(DO>20|is.na(DO),1,0))
  data$TempFlag=with(data,ifelse(Temp<5|Temp>42|is.na(Temp),1,0));
  data$UseData=ifelse(rowSums(data[,c("TimeFlag","DOFlag","TempFlag")])>0,"No","Yes")
  return(data)
}
