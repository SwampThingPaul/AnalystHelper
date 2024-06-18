#' South Florida Water Management District online database (DBHYDRO) metadata
#'
#' @param site SFWMD station id
#' @param type data type ("FLOW","STG","GATE", etc)
#' @param cat data category ("SW","RAIN","ETP"); default is "SW"
#' @param ... other html parameters (i.e. "v_basin=L_OKEE", "v_dbkey_list_flag=Y&v_order_by=STATION","v_station=L001-B")
#' @keywords "water quality"
#' @export
#' @return This function returns water quality dataset from the SFWMD monitoring network (https://apps.sfwmd.gov/WAB/EnvironmentalMonitoring/index.html). This function assumes some familiarity with the District monitoring network and data management. .
#' @importFrom utils browseURL read.csv
#' @importFrom rvest read_html html_table
#'
#' @examples
#' # Water Quality Data
#' # DBHYDRO.meta.bysite("S333","FLOW")



DBHYDRO.meta.bysite=function(site,data_type=NA,cat=NA,freq=NA,stat=NA,returnlink=F,...){
  #station v_station=L001-B
  # site=c("L001","L002")
  # data_type="PU"
  # cat="WQ"
  # freq="DA"
  # stat="MEAN"

  servfull="https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched"

  if(length(site)>1){
    site.vals=paste(paste0("v_site=",site),collapse="&")
  }else{
    site.vals=paste0("v_site=",site)
  }

  qy <- list(v_data_type = data_type,
             v_category = cat,
             v_frequency = freq,
             v_statistic_type = stat
             )

  qy=qy[is.na(qy)==FALSE]

  link=paste0(paste(servfull,site.vals,sep="?"),paste(paste(names(qy),qy,sep="="),collapse="&"))
  # shell.exec(link)

  # if(length(site)>1){
  #   site.vals=paste(paste0("v_site=",site),collapse="&")
  # }else{
  #   site.vals=paste0("v_site=",site)
  # }
  #
  # link=paste0("https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched?v_js_flag=Y&v_category=",cat,"&",
  #             site.vals,
  #             "&v_data_type=",type,
  #             "&v_dbkey_list_flag=Y&v_order_by=STATION",...)
  rslt.table=rvest::read_html(link)
  rslt.table=data.frame(rvest::html_table(rslt.table,fill=T)[[5]])
  rslt.table=rslt.table[,2:ncol(rslt.table)]
  colnames(rslt.table)=toupper(names(rslt.table))
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
