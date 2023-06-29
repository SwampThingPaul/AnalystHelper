#' Data retrieval from South Florida Water Management District online database (DBHYDRO)
#'
#' @param date_min input start date as.Date (YYYY-MM-DD)
#' @param date_max input end date as.Date
#' @param station_id SFWMD Water Quality Monitoring location
#' @param test_number corresponds to SFWMD data management system
#' @param Exclude.FieldQC true/false field
#' @param Exclude.Flagged true/false field
#' @param sample_type default 'SAMP'
#' @param matrix default is "SW" for surface water other variables include "BAL","BAN","DI","BFE","BFI","GW","PERI","BPL","PW","RA","SE","SO","UNK"
#' @param target_code default file_csv but can have "screen" and default browser will launch
#' @param cust_str custom HTML string to add to query, example "+and+station_id+in+('L31NNGW4')" to look for a specific location (if station_id is NULL)
#' @keywords "water quality"
#' @export
#' @return This function returns water quality dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management. .
#' @importFrom utils browseURL read.csv
#'
#' @examples
#' # Water Quality Data
#' sdate=as.Date("2001-05-01");
#' edate=as.Date("2002-05-01");
#' parameter=25;#Test Number for Phosphate, Total as P (mg/L)
#' dat=DBHYDRO_WQ(sdate,edate,"S12A",parameter,target_code="file_csv")

DBHYDRO_WQ=function(date_min,date_max,station_id,test_number,
                    matrix="SW",cust_str=NULL,
                    Exclude.FieldQC = "Y",Exclude.Flagged = "Y",
                    sample_type="SAMP",target_code="file_csv"){
  #version 2.0; includes Flagged, QAQC and sample type toggles
  #Sample Type definitions include SAMP=Sample, RS=Replicate Sample, SS= Split Sample, EB=Equipment Blank, FCEB=Field Cleaned Equipment Blank, FD=Field Duplicate

  servfull <- "http://my.sfwmd.gov/dbhydroplsql/water_quality_data.report_full?"

  date_min <- paste("'",paste(format(date_min,"%d"),toupper(format(date_min,"%b")),format(date_min,"%Y"),sep="-"),"'",sep="")
  date_max <- paste("'",paste(format(date_max,"%d"),toupper(format(date_max,"%b")),format(date_max,"%Y"),sep="-"),"'",sep="")
  if(date_min>date_max){stop("Check dates date_min can't be after date_max")}

  station_list <- paste("(", paste("'", station_id, "'", sep = "",collapse = ","), ")", sep = "")
  station_list <- if(is.null(station_id)==F){paste("station_id", "in",station_list,sep="+")}else{NULL}

  sample_type_new <- paste("(", paste("'", sample_type, "'", sep = "",collapse = ","), ")", sep = "")
  sample_type_new <- if(is.null(sample_type)==F){paste("sample_type_new","in",sample_type_new,sep="+")}else{NULL}

  test_number_val <- paste("(",paste(test_number,collapse=",",sep=""),")",sep="")
  test_number_val <- if(is.null(test_number)==F){paste("test_number","in", test_number_val,sep="+")}else{NULL}

  matrix_val=paste("(", paste("'", matrix, "'", sep = "", collapse = ","),")", sep = "")
  matrix_val= if(is.null(matrix_val)==F){paste("matrix", "in",matrix_val,sep="+")}else{NULL}

  v_where_clause = paste("v_where_clause=where",
                         if(is.null(station_list)==F){paste(station_list,"and",sep="+")},
                         if(is.null(test_number)==F){paste(test_number_val,"and",sep="+")},
                         if(is.null(matrix_val)==F){paste(matrix_val,"and",sep="+")},
                         "date_collected",">=",date_min, "and", "date_collected","<",date_max,
                         if(is.null(sample_type_new)==F){paste("and",sample_type_new,sep="+")}, sep = "+")
  v_where_clause=gsub("\\++","\\+",v_where_clause)
  # v_where_clause = paste("v_where_clause=where","station_id", "in", station_list,"and","test_number",
  #                        "in", test_number,"and",
  #                        "date_collected",">=",date_min, "and", "date_collected","<",date_max,
  #                        "and", "sample_type_new","in",sample_type_new, sep = "+")
  v_target=paste("&","v_exc_qc=",Exclude.FieldQC,"&","v_exc_flagged=",Exclude.Flagged,"&","v_target_code=",target_code,sep="")
  link=paste0(servfull,v_where_clause,if(is.null(cust_str)==T){""},v_target)
  if(target_code=="screen"){browseURL(link)}else{

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
}

#' @export
SFWMD.DBHYDRO.Data.WQ=function(date_min,date_max,station_id,test_number,Exclude.FieldQC = "Y",Exclude.Flagged = "Y",sample_type="SAMP"){
  dat=DBHYDRO_WQ(date_min,date_max,station_id,test_number,Exclude.FieldQC,Exclude.Flagged,sample_type)
  warning("This function is being phased out. Consider using DBHYDRO_WQ in the future.")
  return(dat)
}
