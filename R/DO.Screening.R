#' Function to screen dissolved oxygen data within the Everglades Ecosystem prior to evaluating compliance with established water quality standards.
#'
#' @param DateTime as.POSIXct date and time
#' @param DO Dissolved Oxygen Concentration
#' @param Temp Surface water temperature
#' @param ... specify other parameters as needed (min.hour,max.hour,max.DO,min.Temp,max.Temp)
#' @param data data.frame() with DateTime (DateTime.EST), Dissolved Oxygen (concentration; DO) and Temperature (Temp) data.
#' @param hour.lims min and max hour to capture "day-time" DO
#' @param max.DO Limit to the maximum observed DO for the system
#' @param temp.lims limits to water temperature data that seem reasonable based on period of record.
#' @keywords Oxygen
#' @export
#' @return   DO.Screening() assumes the data has a "DateTime.EST" field (date and time stamp), "DO" Field and "Temp" field to determine suitable data. Returns a "UseData" response.
#' @examples
#' data=data.frame(DateTime.EST=as.POSIXct(c("2015-05-01 08:00","2015-10-15 12:00","2015-10-15 5:00")),
#' DO=c(15,2,3),Temp=c(18,27,3))
#'
#' data$UseData=DO.Screening(DateTime.EST,DO,Temp,data)
#' data

DO.Screening=function(DateTime,DO,Temp,data,hour.lims=c(6,18),max.DO=20,temp.lims=c(5,20)){
  if(!missing(data)){
    DateTime.EST=data[,deparse(substitute(DateTime))]
    DO=data[,deparse(substitute(DO))]
    Temp=data[,deparse(substitute(Temp))]
  }
  min.hour=hour.lims[1]
  max.hour=hour.lims[2]
  max.DO=max.DO
  min.Temp=temp.lims[1]
  max.Temp=temp.lims[2]

  TimeFlag=ifelse(as.numeric(format(DateTime.EST,"%H"))==0,1,ifelse(as.numeric(format(DateTime.EST,"%H"))<min.hour|as.numeric(format(DateTime.EST,"%H"))>max.hour,1,0))
  DOFlag=ifelse(DO>max.DO|is.na(DO),1,0)
  TempFlag=ifelse(Temp<min.Temp|Temp>max.Temp|is.na(Temp),1,0)
  UseData=ifelse(TimeFlag+DOFlag+TempFlag>0,"No","Yes")
  return(UseData)
}



