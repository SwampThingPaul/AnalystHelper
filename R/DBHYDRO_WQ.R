#' Data retrievial from South Florida Water Management District online database (DBHYDRO)
#'
#' @param date_min input start date as.Date (YYYY-MM-DD)
#' @param date_max input end date as.Date
#' @param station_id SFWMD Water Quality Monitoring location
#' @param test_number corresponds to SFWMD data management system
#' @param Exclude.FieldQC true/false field
#' @param Exclude.Flagged true/false field
#' @param sample_type default 'SAMP'
#' @keywords "water quality"
#' @export
#' @return This function returns water quality dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management. .
#' @examples
#' # Water Quality Data
#' sdate=as.Date("2001-05-01");
#' edate=as.Date("2002-05-01");
#' parameter=25;#Test Number for Phosphate, Total as P (mg/L)
#' dat=DBHYDRO_WQ(sdate,edate,"S12A",parameter)

DBHYDRO_WQ=function(date_min,date_max,station_id,test_number,Exclude.FieldQC = "Y",Exclude.Flagged = "Y",sample_type="SAMP"){
  #version 2.0; includes Flagged, QAQC and sample type toggles
  #Sample Type definitions include SAMP=Sample, RS=Replicate Sample, SS= Split Sample, EB=Equipment Blank, FCEB=Field Cleaned Equipment Blank, FD=Field Duplicate

  servfull <- "http://my.sfwmd.gov/dbhydroplsql/water_quality_data.report_full?"

  station_list <- paste("(", paste("'", station_id, "'", sep = "",collapse = ","), ")", sep = "")
  date_min <- paste("'",paste(format(date_min,"%d"),toupper(format(date_min,"%b")),format(date_min,"%Y"),sep="-"),"'",sep="")
  date_max <- paste("'",paste(format(date_max,"%d"),toupper(format(date_max,"%b")),format(date_max,"%Y"),sep="-"),"'",sep="")
  sample_type_new<-{paste("(", paste("'", sample_type, "'", sep = "",collapse = ","), ")", sep = "")}
  test_number=paste("(",paste(test_number,collapse=",",sep=""),")",sep="")

  v_where_clause = paste("v_where_clause=where","station_id", "in", station_list,"and","test_number",
                         "in", test_number,"and",
                         "date_collected",">=",date_min, "and", "date_collected","<",date_max,
                         "and", "sample_type_new","in",sample_type_new, sep = "+")
  v_target=paste("&","v_exc_qc=",Exclude.FieldQC,"&","v_exc_flagged=",Exclude.Flagged,"&","v_target_code=file_csv",sep="")
  link=paste0(servfull,v_where_clause,v_target)

  REPORT=read.csv(link);
  REPORT=subset(REPORT,is.na(Test.Number)==F)
  REPORT$Collection_Date=as.POSIXct(strptime(REPORT$Collection_Date,"%d-%b-%Y %R"),tz="America/New_York")
  REPORT$First.Trigger.Date=as.POSIXct(strptime(REPORT$First.Trigger.Date,"%d-%b-%Y %R"),tz="America/New_York")
  REPORT$Date=as.POSIXct(strptime(REPORT$Collection_Date,"%F"),tz="America/New_York")
  REPORT$DateTime.EST=REPORT$Collection_Date
  attr(REPORT$DateTime.EST,"tzone")<-"EST"
  REPORT$Date.EST=as.POSIXct(strptime(REPORT$DateTime.EST,"%F"),tz="EST")
  REPORT$HalfMDL=with(REPORT,ifelse(Value<0,abs(Value)/2,Value))
  return(REPORT)
}

SFWMD.DBHYDRO.Data.WQ=function(date_min,date_max,station_id,test_number,Exclude.FieldQC = "Y",Exclude.Flagged = "Y",sample_type="SAMP"){
  #.Deprecated("DBHYDRO_WQ")
  dat=DBHYDRO_WQ(date_min,date_max,station_id,test_number,Exclude.FieldQC,Exclude.Flagged,sample_type)
  warning("This function is being phased out. Consider using DBHYDRO_WQ in the future.")
  return(dat)
}
