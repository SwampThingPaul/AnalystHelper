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

DBHYDRO.meta.bysite=function(site=NA,station=NA,data_type=NA,cat=NA,freq=NA,stat=NA,returnlink=F,add_arg = NA){
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
  if(!anyNA(add_arg)){
    qy <- c(qy,add_arg)
  }

  qy=qy[is.na(qy)==FALSE]
  if(sum(names(qy)%in%c('v_site'))==1){qy$v_site=paste(paste0("v_site=",qy$v_site),collapse="&")}
  if(sum(names(qy)%in%c('v_station'))==1){qy$v_station=paste(paste0("v_station=",qy$v_station),collapse="&")}

  qy2=qy[!(names(qy)%in%c("v_site","v_station"))] # other variables
  qy=qy[names(qy)%in%c("v_site","v_station")] # site and/or station variables only


  link=paste0(paste0(servfull,"?"),paste(qy,collapse="&"),"&",paste(paste(names(qy2),qy2,sep="="),collapse="&"))
  # shell.exec(link); # for testing

  ## old basic code
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
  rslt.table=rslt.table[,!(names(rslt.table)%in%c("GetData"))]
  # rslt.table=rslt.table[,2:ncol(rslt.table)]
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
