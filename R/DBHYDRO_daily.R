#' Data retrievial from South Florida Water Management District online database (DBHYDRO)
#'
#' @param SDATE input start date as.Date (YYYY-MM-DD)
#' @param EDATE input end date as.Date (YYYY-MM-DD)
#' @param DBK SFWMD daily data site and data type identifier
#' @param offset the values used to remove the data file header.
#' @keywords discharge weather stage
#' @return This function returns daily hydrometerological (discharge, stage(WL) and meterological parameters) dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management.
#' @importFrom utils read.csv
#' @export
#' @examples
#' \dontrun{
#' # Daily Discharge Data
#' sdate=as.Date("2001-05-01");
#' edate=as.Date("2002-05-01");
#' dat=DBHYDRO_daily(SDATE,EDATE,"FE771")
#' }

DBHYDRO_daily=function(SDATE,EDATE,DBK,offset=4){
  #Returns daily data from SFWMD DBHydro
  DBK.val=paste("",DBK,"",collapse="/",sep="")
  SDATE=paste(format(SDATE,"%Y"),toupper(format(SDATE,"%m")),format(SDATE,"%d"),sep="");#In YYYYMMDD format
  EDATE=paste(format(EDATE,"%Y"),toupper(format(EDATE,"%m")),format(EDATE,"%d"),sep="");#In YYYYMMDD format
  link=paste("http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date=",SDATE,"&v_end_date=",EDATE,"&v_report_type=format6&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_dbkey=",DBK.val,sep="")
  REPORT=read.csv(link,skip=length(DBK)+offset)
  REPORT$Date=with(REPORT,as.POSIXct(as.character(Daily.Date),format="%d-%b-%Y",tz="America/New_York"))
  REPORT=subset(REPORT,is.na(Date)==F)
  return(REPORT)
}

#' @export
SFWMD.DBHYDRO.Data.daily=function(SDATE,EDATE,DBK){
  dat=DBHYDRO_daily(SDATE,EDATE,DBK)
  warning("This function is being phased out. Consider using DBHYDRO_daily in the future.")
  return(dat)
}
