#' Data retrievial from South Florida Water Management District online database (DBHYDRO)
#'
#' @param channel The path to the access (.accdb) files with extension
#' @param table Table within databases. If unsure check RODBC::sqlTables()
#' @keywords access
#' @export
#' @return This function allows for quick access of data within an 2007/2010 Microsoft Access database (.accdb).
#' @examples
#'
#' dat=read.access("/database.accdb","WaterQuality")
#'


read.access=function(channel,table){
  require(RODBC)
  chan=odbcConnectAccess2007(channel)
  dat=sqlFetch(chan,table)
  close(chan)
  return(dat)
}
