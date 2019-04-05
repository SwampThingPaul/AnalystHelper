#' Function to make folders within working directory.
#'
#' @param x a directory where you want a list of folders created.
#' @keywords files
#' @export
#' @return   Creates folders and will not overwrite any existing folders
#' @examples
#' # paths=paste(getwd(),c("maps","plots","data"),sep="/")
#' # Folder.Maker(paths)

Folder.Maker=function(x){
  for(i in 1:length(x)){
    if(dir.exists(x[i])==F){dir.create(x[i])}else{print(paste("Directory",x[i],"exists"))}
  }
}
