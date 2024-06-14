#' Date function to convert character/other as.POSIXct to date.
#'
#' @param x as.POSIXct or as.character
#' @param tz timezone (default set to Eastern Standard Time)
#' @param form format (default set to YYYY-mm-dd)
#' @keywords date
#' @export
#' @return Helper function to format date (and date-time) fields
#' @examples
#' datetime=as.character(c("2015-05-01 08:00:00","2015-10-15 12:00:00","2015-10-15 5:00:00"))
#' date.fun(datetime);# just date
#' date.fun(datetime, form="%F %X"); #include date and time
#'
#' date.fun("2020-05-01")
#' date.fun(as.Date("2020-05-01"))
#' date.fun(as.POSIXct("2020-05-01 12:00:00",tz="EST"),form="%F %H:%M:%S")
#' date.fun(as.POSIXct("2020-05-01 12:00:00",tz="UTC"),form="%F %H:%M:%S")

date_fun=function(x,tz="EST",form="%F"){
  # as.POSIXct(strptime(x,form),tz=tz)
  date.fun(x,tz=tz,form=form)
}

#' @export date.fun
date.fun=function(x,tz="EST",form="%F"){
  # Original code ... doesn't correct for changes in timezone
  # as.POSIXct(strptime(x,form),tz=tz)
  if(is.null(attr(x,"tzone"))&is.null(tz)){stop("Define Time Zone or set tz value")}
  if(is.null(attr(x,"tzone"))&is.null(tz)==F){as.POSIXct(strptime(x,format=form),tz=tz)
  }else if(attr(x,"tzone")!=tz){
    if(grepl("%X|%H|%M|%I",form)){
      attr(x,"tzone")<-tz
      as.POSIXct(strptime(x,format=form),tz=tz)
    }else{
      as.POSIXct(strptime(x,format=form),tz=tz)
    }
  }else{
    as.POSIXct(strptime(x,format=form),tz=tz)
  }

}
