#' South Florida Water Management District online database (DBHYDRO) metadata
#'
#' @param site SFWMD station id
#' @param type data type ("FLOW","STG","GATE", etc)
#' @param cat data category ("SW","RAIN","ETP"); default is "SW"
#' @param ... other html parameters
#' @keywords "water quality"
#' @export
#' @return This function returns water quality dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management. .
#' @importFrom utils browseURL read.csv
#' @importFrom rvest read_html
#'
#' @examples
#' # Water Quality Data
#' # DBHYDRO.meta.bysite("S333","FLOW")



DBHYDRO.meta.bysite=function(site,type,cat="SW",...){
  # c("FLOW","STG","GATE")
  # cat = c("SW","RAIN","ETP")
  # site.vals=paste0("v_site=",site)
  if(length(site)>1){
    site.vals=paste(paste0("v_site=",site),collapse="&")
  }else{
    site.vals=paste0("v_site=",site)
  }

  link=paste0("https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched?v_js_flag=Y&v_category=",cat,"&",
              site.vals,
              "&v_data_type=",type,
              "&v_dbkey_list_flag=Y&v_order_by=STATION",...)
  rslt.table=read_html(link)
  rslt.table=data.frame(html_table(rslt.table,fill=T)[[5]])
  rslt.table=rslt.table[,2:ncol(rslt.table)]
  colnames(rslt.table)=toupper(names(rslt.table))
  return(rslt.table)

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
