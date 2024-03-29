#' Data retrievial from South Florida Water Management District online database (DBHYDRO)
#'
#' @param SDATE input start date as.Date (YYYY-MM-DD)
#' @param EDATE input end date as.Date (YYYY-MM-DD)
#' @param DBK SFWMD daily data site and data type identifier
#' @keywords discharge weather stage
#' @export
#' @return This function returns breakpoint (i.e. 15-minute) hydrometerological (discharge, stage(WL) and meterological parameters) dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management.
#' @importFrom utils read.csv
#' @examples
#' \dontrun{
#' # Daily Discharge Data
#' sdate=as.Date("2001-05-01");
#' edate=as.Date("2001-06-01");
#' dat=DBHYDRO_breakpoint(SDATE,EDATE,"AI516")
#' }
DBHYDRO_breakpoint=function(SDATE,EDATE,DBK,col.names=c("DATETIME","Station","DBKEY","Data.Value","Flag","Comment"),timeout=200){
  DBK.val=paste("",DBK,"",collapse="/",sep="")
  SDATE=paste(format(SDATE,"%Y"),toupper(format(SDATE,"%m")),format(SDATE,"%d"),sep="");#In YYYYMMDD format
  EDATE=paste(format(EDATE,"%Y"),toupper(format(EDATE,"%m")),format(EDATE,"%d"),sep="");#In YYYYMMDD format
  link=paste("http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date=",SDATE,"&v_end_date=",EDATE,"&v_report_type=format6&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_dbkey=",DBK.val,sep="")

  tmp=RCurl::getURL(link,timeout=timeout)
  REPORT=read.csv(textConnection(tmp),skip=length(DBK)+2,col.names=col.names,header=F)
  REPORT$DATETIME=as.POSIXct(REPORT$DATETIME,format="%d-%b-%Y %H:%M",tz="EST")
  REPORT$DATE=as.POSIXct(format(REPORT$DATETIME,format="%Y-%m-%d"),tz="EST")
  REPORT=subset(REPORT,is.na(DATETIME)==F)
  return(REPORT)
}

#' @export
SFWMD.DBHYDRO.Data.breakpoint=function(SDATE,EDATE,DBK,col.names=c("DATETIME","Station","DBKEY","Data.Value","Flag","Comment")){
  dat=DBHYDRO_breakpoint(SDATE,EDATE,DBK,col.names)
  warning("This function is being phased out. Consider using DBHYDRO_breakpoint in the future.")
  return(dat)
}

# DBHYDRO_breakpoint=function(SDATE,EDATE,DBK,col.names=c("DATETIME","Station","DBKEY","Data.Value","Flag","Comment")){
#   DBK.val=paste("",DBK,"",collapse="/",sep="")
#   SDATE=paste(format(SDATE,"%Y"),toupper(format(SDATE,"%m")),format(SDATE,"%d"),sep="");#In YYYYMMDD format
#   EDATE=paste(format(EDATE,"%Y"),toupper(format(EDATE,"%m")),format(EDATE,"%d"),sep="");#In YYYYMMDD format
#   link=paste("http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date=",SDATE,"&v_end_date=",EDATE,"&v_report_type=format6&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_dbkey=",DBK.val,sep="")
#   REPORT=read.csv(link,skip=length(DBK)+2,col.names=col.names,header=F)
#   REPORT$DATETIME=as.POSIXct(REPORT$DATETIME,format="%d-%b-%Y %H:%M",tz="EST")
#   REPORT$DATE=as.POSIXct(format(REPORT$DATETIME,format="%Y-%m-%d"),tz="EST")
#   REPORT=subset(REPORT,is.na(DATETIME)==F)
#   return(REPORT)
# }
