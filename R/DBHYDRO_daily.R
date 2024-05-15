#' Data retrieval from South Florida Water Management District online database (DBHYDRO)
#'
#' @param SDATE input start date as.Date (YYYY-MM-DD)
#' @param EDATE input end date as.Date (YYYY-MM-DD)
#' @param DBK SFWMD daily data site and data type identifier
#' @param dataonly DEFAULT is TRUE to return a data.frame of data, if set to FALSE it will return a nested list of two data.frame labeled METADATA and REPORT.
#' @param period DEFAULT set to "uspec" for URL query
#' @param v_target_code DEFAULT set to "file_csv" to return a CSV file
#' @param vert_datum DEFAULT set to 1 for NGVD29 vertical datum; 2 = NAVD88 vertical datum
#' @param ... to allow for more functionality and flexibility in building URL queries
#' @keywords discharge weather stage
#' @return This function returns daily hydrometerological (discharge, stage(WL) and meterological parameters) dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management.
#' @importFrom utils read.csv
#' @export
#' @examples
#' \dontrun{
#' # Daily Discharge Data
#' sdate=as.Date("2001-05-01");
#' edate=as.Date("2002-05-01");
#' dat=DBHYDRO_daily(sdate,edate,"FE771")
#' }

DBHYDRO_daily=function(SDATE, EDATE, DBK,dataonly=TRUE,period = "uspec",v_target_code = "file_csv",vert_datum=1,...)
{
  # Legacy Code
  #Returns daily data from SFWMD DBHydro
  # DBK.val=paste("",DBK,"",collapse="/",sep="")
  # SDATE=paste(format(SDATE,"%Y"),toupper(format(SDATE,"%m")),format(SDATE,"%d"),sep="");#In YYYYMMDD format
  # EDATE=paste(format(EDATE,"%Y"),toupper(format(EDATE,"%m")),format(EDATE,"%d"),sep="");#In YYYYMMDD format
  # link=paste("http://my.sfwmd.gov/dbhydroplsql/web_io.report_process?v_period=uspec&v_start_date=",SDATE,"&v_end_date=",EDATE,"&v_report_type=format6&v_target_code=file_csv&v_run_mode=onLine&v_js_flag=Y&v_dbkey=",DBK.val,sep="")
  # REPORT=read.csv(link,skip=length(DBK)+offset)
  # REPORT$Date=with(REPORT,as.POSIXct(as.character(Daily.Date),format="%d-%b-%Y",tz="America/New_York"))
  # REPORT=subset(REPORT,is.na(Date)==F)
  # return(REPORT)
  # offset variable is a legecy

  # Code inspired by dbhydroR
  # period <- "uspec"
  # v_target_code <- "file_csv"
  # vert_datum <- 1 #1 = NGVD29; 2 = NAVD88

  # Add warning about date format
  # if(is.date(SDATE)==F|is.POSIXct(SDATE)==F|
  #    is.date(EDATE)==F|is.POSIXct(EDATE)==F){# |!(nchar(SDATE)==10&nchar(EDATE)==10)){
  #   stop("Enter dates as a date. ")
  #   # stop("Enter dates as character strings in YYYY-MM-DD format or as.Date(...)")
  # }
  if(all(is.na(DBK))==T){
    stop("Must specify either a dbkey")
  }

  DBK.val=paste("",DBK,"",collapse="/",sep="")
  SDATE=paste(format(SDATE,"%Y"),toupper(format(SDATE,"%m")),format(SDATE,"%d"),sep="");#In YYYYMMDD format
  EDATE=paste(format(EDATE,"%Y"),toupper(format(EDATE,"%m")),format(EDATE,"%d"),sep="");#In YYYYMMDD format

  qy <- list(v_period = period, v_start_date = SDATE, v_end_date = EDATE,
             v_report_type = "format6", v_target_code = v_target_code,
             v_run_mode = "onLine", v_js_flag = "Y", v_dbkey = DBK.val,v_datum = vert_datum,...)
  qy=qy[is.na(qy)==FALSE]

  servfull <- "http://my.sfwmd.gov/dbhydroplsql/web_io.report_process"

  link=paste(servfull,paste(paste(names(qy),qy,sep="="),collapse="&"),sep="?")

  if(v_target_code=="screen"){browseURL(link)}else{

  res=readLines(link)

  raw <- suppressMessages(read.csv(text = res, skip = 1,stringsAsFactors = FALSE, row.names = NULL))
  base_skip <- 1
  i= 1 + min(which(apply(raw[,10:16], 1, function(x) all(is.na(x) |nchar(x) == 0))))

  metadata <- suppressMessages(read.csv(text = res, skip = base_skip,stringsAsFactors = FALSE, row.names = NULL))[1:(i - 1),]
  name.vals=names(metadata)
  metadata <- subset(metadata,is.na(AGENCY)==F)[,1:(ncol(metadata)-1)]
  names(metadata) <- c(name.vals[2:(length(name.vals))])
  metadata

  # dat.col.names=as.character(raw[i,])# dput(as.character(raw[i,]))
  # dat.col.names=dat.col.names[!(dat.col.names%in%c("NA",""))]
  # dat.col.names=gsub(" ",".",dat.col.names);# just incase

  if(metadata$FQ%in%c("BK")){
    head.val=c("DATETIME","Station","DBKEY","Data.Value","Flag","Comment")

    REPORT=suppressMessages(read.csv(text = res, skip = i+1,stringsAsFactors = FALSE, row.names = NULL,col.names = head.val))
    REPORT$DATETIME=as.POSIXct(REPORT$DATETIME,format="%d-%b-%Y %H:%M",tz="EST")
    REPORT$DATE=as.POSIXct(format(REPORT$DATETIME,format="%Y-%m-%d"),tz="EST")
    REPORT=subset(REPORT,is.na(DATE)==F);# clean up
  }else{
    REPORT=suppressMessages(read.csv(text = res, skip = i+1,stringsAsFactors = FALSE, row.names = NULL))
    REPORT$Daily.Date=with(REPORT,as.POSIXct(as.character(Daily.Date),format="%d-%b-%Y",tz="America/New_York"))
    REPORT$Date=REPORT$Daily.Date;# legacy variable
    REPORT$Revision.Date=with(REPORT,as.POSIXct(as.character(Revision.Date),format="%d-%b-%Y",tz="America/New_York"))
    REPORT=subset(REPORT,is.na(Date)==F);# clean up
  }

  final=list(METADATA = metadata,REPORT = REPORT)

  if(dataonly==TRUE){
    return(final$REPORT)
  }else{final}
}
}

#' @export
SFWMD.DBHYDRO.Data.daily=function(SDATE,EDATE,DBK){
  .Deprecated('SFWMD.DBHYDRO.Data.daily')
  # dat=DBHYDRO_daily(SDATE,EDATE,DBK)
  # warning("This function is being phased out. Consider using DBHYDRO_daily in the future.")
  # return(dat)
}

#' @export
is.date = function(x) inherits(x,"Date")
