#' Data retrieval from South Florida Water Management District online database (DBHYDRO)
#'
#' @param date_min input start date as.Date (YYYY-MM-DD)
#' @param date_max input end date as.Date
#' @param station_id SFWMD Water Quality Monitoring location, see details
#' @param test_number corresponds to SFWMD data management system
#' @param Exclude.FieldQC true/false field, DEFAULT is TRUE
#' @param Exclude.Flagged true/false field, DEFAULT is TRUE
#' @param sample_type default 'SAMP'
#' @param matrix default is "SW" for surface water
#' @param target_code default file_csv but can have "screen" and default browser will launch
#' @param collect_method default is NULL but can specify G, GP, ACF, ACT, ADT
#' @param cust_str custom sql query string to add to link (not implemented)
#' @keywords "water quality"
#' @export
#' @return This function returns water quality dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management. .
#' @importFrom utils browseURL read.csv
#' @details
#' Sample Type definitions include SAMP=Sample, RS=Replicate Sample, SS= Split Sample, EB=Equipment Blank, FCEB=Field Cleaned Equipment Blank, FD=Field Duplicate
#'
#' matrix default is "SW" for surface water other variables including "BAL","BAN","DI","BFE","BFI","GW","PERI","BPL","PW","RA","SE","SO","UNK"
#'
#' target_code can be either "file_csv" (for CSV) and "screen" (launch default browser). Other formats like "file_txt" (for fixed column width) and "file_pdf" (for pdf) and "screen" (launch default browser) have not been implemented
#'
#' station_id can use wildcard character but not in combination with wildcard and non-wildcard (i.e. S12%, S333)
#'
#' @examples
#' # Water Quality Data
#' \dontrun{
#' sdate=as.Date("2001-05-01");
#' edate=as.Date("2002-05-01");
#' parameter=25;#Test Number for Phosphate, Total as P (mg/L)
#' dat=DBHYDRO_WQ(sdate,edate,"S12%",parameter,target_code="file_csv")
#' }

DBHYDRO_WQ=function(date_min,date_max,station_id=NA,
                    test_number=NA,collect_method=NULL,
                    matrix="SW",cust_str=NULL,
                    Exclude.FieldQC = T,Exclude.Flagged = T,
                    sample_type="SAMP",target_code="file_csv"){

  servfull <- "http://my.sfwmd.gov/dbhydroplsql/water_quality_data.report_full?"

  if(date_min>date_max){stop("Check dates date_min can't be after date_max")}
  if(is.na(test_number)==T){stop("Need to specify test number")}

  ## Date Formatting
  date_min <- paste("'",paste(format(date_min,"%d"),toupper(format(date_min,"%b")),format(date_min,"%Y"),sep="-"),"'",sep="")
  date_max <- paste("'",paste(format(date_max,"%d"),toupper(format(date_max,"%b")),format(date_max,"%Y"),sep="-"),"'",sep="")

  ## To account for wildcard searching
  station_like <- station_id[grepl("%", station_id)]
  station_like <- gsub("%","%25",station_like)
  stat.like.N=length(station_like[!(is.na(station_like))])
  if(stat.like.N > 0){
    station_id <- station_id[!grepl("%", station_id)]

    station_like <- paste("(", paste("'", station_like, "'", sep = "",collapse = ","), ")", sep = "")
    station_like_val <- if(is.null(station_like)==F){paste("station_id", "like",station_like,sep="+")}
  }else{
    station_like <- NA
  }

  ## recycled code form last version
  station_list <- paste("(", paste("'", station_id, "'", sep = "",collapse = ","), ")", sep = "")
  station_list <- if(is.null(station_id)==F){paste("station_id", "in",station_list,sep="+")}else{NULL}

  sample_type_new <- paste("(", paste("'", sample_type, "'", sep = "",collapse = ","), ")", sep = "")
  sample_type_new <- if(is.null(sample_type)==F){paste("sample_type_new","in",sample_type_new,sep="+")}else{NULL}

  test_number_val <- paste("(",paste(test_number,collapse=",",sep=""),")",sep="")
  test_number_val <- if(is.null(test_number)==F){paste("test_number","in", test_number_val,sep="+")}else{NULL}

  # Not implemented
  # test_name_val <- paste("(",paste(test_name,collapse=",",sep=""),")",sep="")
  # test_name_val <- if(is.null(test_name)==F){paste("test_name","in", test_name,sep="+")}else{NULL}

  matrix_val=paste("(", paste("'", matrix, "'", sep = "", collapse = ","),")", sep = "")
  matrix_val= if(is.null(matrix_val)==F){paste("matrix", "in",matrix_val,sep="+")}else{NULL}

  collect_method_val <- paste("(", paste("'", collect_method, "'", sep = "", collapse = ","),")", sep = "")
  collect_method_val <- if(is.null(collect_method_val)==F){paste("collect_method", "in",collect_method_val,sep="+")}else{NULL}

  Exclude.FieldQC <- if(Exclude.FieldQC == TRUE){"Y"}else{"N"}
  Exclude.Flagged <- if(Exclude.Flagged == TRUE){"Y"}else{"N"}

  if( stat.like.N >0 & any(!is.na(station_like))){
    qy <- list(v_where_clause = paste("where",
                                      if(is.null(station_like)==F){paste(station_like_val,"and",sep="+")},
                                      if(is.null(test_number)==F){paste(test_number_val,"and",sep="+")},#else{test_name_val},
                                      if(is.null(matrix_val)==F){paste(matrix_val,"and",sep="+")},
                                      if(is.null(collect_method)==F){paste(collect_method_val,"and",sep="+")},
                                      "date_collected",">=",date_min, "and", "date_collected","<",date_max,
                                      if(is.null(sample_type_new)==F){paste("and",sample_type_new,sep="+")}, sep = "+"),
               v_exc_qc = Exclude.FieldQC, v_exc_flagged = Exclude.Flagged,v_target_code = target_code)
  }else{
    qy <- list(v_where_clause = paste("where",
                                      if(is.null(station_list)==F){paste(station_list,"and",sep="+")},
                                      if(is.null(test_number)==F){paste(test_number_val,"and",sep="+")},#else{test_name_val},
                                      if(is.null(matrix_val)==F){paste(matrix_val,"and",sep="+")},
                                      if(is.null(collect_method)==F){paste(collect_method_val,"and",sep="+")},
                                      "date_collected",">=",date_min, "and", "date_collected","<",date_max,
                                      if(is.null(sample_type_new)==F){paste("and",sample_type_new,sep="+")}, sep = "+"),
               v_exc_qc = Exclude.FieldQC, v_exc_flagged = Exclude.Flagged,v_target_code = target_code)
  }
  qy$v_where_clause=gsub("\\++","\\+",qy$v_where_clause);# incase there is a null and it removes ++
  qy=paste(names(qy),qy,sep="=")

  # link=paste0(servfull,qy[1],paste(paste(names(qy[2:4]),qy[2:4],sep="="),collapse="&"))
  if(is.null(cust_str)==T){
    link=paste0(servfull,paste(qy[1],paste(qy[2:4],collapse="&"),sep="&"))
  }else{
    link=paste0(servfull,paste(qy[1],cust_str,paste(qy[2:4],collapse="&"),sep="&"))
  }
  if(target_code=="screen"){browseURL(link)}else{
    res=readLines(link)

    REPORT=read.csv(text=res);
    REPORT=subset(REPORT,is.na(Test.Number)==F)
    REPORT$Collection_Date=as.POSIXct(strptime(REPORT$Collection_Date,"%d-%b-%Y %R"),tz="America/New_York")
    REPORT$First.Trigger.Date=as.POSIXct(strptime(REPORT$First.Trigger.Date,"%d-%b-%Y %R"),tz="America/New_York")
    REPORT$Date=as.POSIXct(strptime(REPORT$Collection_Date,"%F"),tz="America/New_York")
    REPORT$DateTime.EST=REPORT$Collection_Date
    attr(REPORT$DateTime.EST,"tzone")<-"EST"
    REPORT$Date.EST=as.POSIXct(strptime(REPORT$DateTime.EST,"%F"),tz="EST")
    # REPORT[,c("Collection_Date","DateTime.EST","Date.EST")]
    REPORT$HalfMDL=with(REPORT,ifelse(Value<0,abs(Value)/2,Value))

    return(REPORT)
  }
}

#' @export
SFWMD.DBHYDRO.Data.WQ=function(date_min,date_max,station_id,test_number,Exclude.FieldQC = "Y",Exclude.Flagged = "Y",sample_type="SAMP"){
  .Deprecated(SFWMD.DBHYDRO.Data.WQ)
  # dat=DBHYDRO_WQ(date_min,date_max,station_id,test_number,Exclude.FieldQC,Exclude.Flagged,sample_type)
  # warning("This function is being phased out. Consider using DBHYDRO_WQ in the future.")
  # return(dat)
}
