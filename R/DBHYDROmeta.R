#' South Florida Water Management District online database (DBHYDRO) metadata
#'
#' @param site SFWMD station id
#' @param type data type ("FLOW","STG","GATE", etc)
#' @param cat data category ("SW","RAIN","ETP"); default is "SW"
#' @param add_arg other html parameters as a list (i.e. list(v_basin="L_OKEE", v_dbkey_list_flag="Y",v_order_by="STATION")
#' @keywords "water quality"
#' @export
#' @return This function returns water quality dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management. .
#' @importFrom utils browseURL read.csv
#' @importFrom rvest read_html html_table
#'
#' @examples
#' # Water Quality Data
#' # DBHYDRO.meta.bysite(site="S333",data_type="FLOW")
#' # DBHYDRO.meta.bysite(c("S11A", "S11B", "S11C"),data_type = "FLOW",freq="DA",add_arg=list(v_agency="COE",v_order_by="STATION", v_dbkey_list_flag="Y"))

DBHYDRO.meta.bysite <-function(site=NA,station=NA,data_type=NA,cat=NA,freq=NA,stat=NA,returnlink=F,add_arg = NA){
  ## for testing
  # site=NA#c("L001","L002")
  # station=c("L001-B","L002-B")
  # data_type="PU"
  # cat="WQ"
  # freq="DA"
  # stat="MEAN"

  servfull="https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched"


  qy <- list(v_site=site,
             v_station=station,
             v_data_type = data_type,
             v_category = cat,
             v_frequency = freq,
             v_statistic_type = stat
  )
  # Merge any additional arguments
  if(!anyNA(add_arg)){
    qy <- c(qy,add_arg)
  }

  # Remove NA entries
  qy <- Filter(Negate(function(x) is.null(x) || is.na(x)), qy)

  # Helper function to encode vector as query string
  encode_query_param <- function(name, value) {
    if (length(value) > 1) {
      paste0(name, "=", paste(value, collapse = "%2F"))  # URL-encoded "/"
    } else {
      paste0(name, "=", value)
    }
  }

  # Encode v_site and v_station if present
  for (param in c("v_site", "v_station")) {
    if (param %in% names(qy)) {
      qy[[param]] <- encode_query_param(param, qy[[param]])
    }
  }

  station_fields <- c("v_site", "v_station")

  qy_station <- qy[names(qy) %in% station_fields]
  qy_other   <- qy[!names(qy) %in% station_fields]

  link <- paste0(
    servfull, "?",
    paste(qy_station, collapse = "&"),
    if (length(qy_other) > 0) paste0("&", paste(names(qy_other), qy_other, sep = "=", collapse = "&")) else ""
  )

  rslt.table <- rvest::read_html(link)
  rslt.table <- data.frame(rvest::html_table(rslt.table,fill=T)[[5]])
  rslt.table <- subset(rslt.table, select = -c(GetData), drop = FALSE)
  colnames(rslt.table) <- toupper(names(rslt.table))
  if(returnlink==TRUE){print(link)}else{
    return(rslt.table)
  }


}


#' South Florida Water Management District online database (DBHYDRO) metadata
#'
#' @param DBKEY SFWMD station id
#' @param ... other html parameters
#' @keywords "water quality"
#' @export
#' @return This function returns water quality dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management. .
#' @importFrom utils browseURL read.csv
#' @importFrom rvest read_html
#'
#' @examples
#' # Water Quality Data
#' # DBHYDRO.meta.byDBKEY(67486)



DBHYDRO.meta.byDBKEY=function(DBKEY,...){

  link=paste0("https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched?v_js_flag=Y&v_dbkey=",
              paste(DBKEY,collapse="/"),...)
  rslt.table=read_html(link)
  rslt.table=data.frame(html_table(rslt.table,fill=T)[[5]])
  rslt.table=rslt.table[,2:ncol(rslt.table)]
  colnames(rslt.table)=toupper(names(rslt.table))
  return(rslt.table)

}
