

#' @title Create a windrose Object
#'
#' @description Create a wind-rose object
#'
#' @param x The x component of wind speed, in which case the u and v components of that object are used for the components of wind speed, and y here is ignored.
#' @param y The y component of wind speed (or stress).
#' @param data Identifies dataframe
#' @param dtheta The angle increment (in degrees) within which to classify the data.
#'
#' @return a windrose object, like oce::as.windrose with data slot containing
#' \itemize{
#'  \item `n` - the number of x values
#'  \item `x.mean` - the mean of hte x values
#'  \item `y.mean` - the mean of the y values
#'  \item `theta` - the central angel (in degrees for the class)
#'  \item `count` - the number of observation in this class
#'  \item `mean` - the mean of the observations in this class
#'  \item `fivenum` - the `fivenum()` vector for observations in this class (min, the hinge, median, upper hinge and max)
#'  }
#' @export
#'
#' @examples
#' \dontrun{
#' library(reshape2)
#' #' dates <- rep(as.Date(Sys.Date())-1,2)
#' dbkeys.bk <- data.frame(SITE="LZ40",DBKEY=c("IY031","IY032"),param=c("WNDD","WNDS"))
#'
#' wnd.dat2 <- insight_fetch_daily(dates[1],dates[2],dbkeys.bk$DBKEY)
#' wnd.dat2 <- merge(wnd.dat2,dbkeys.bk,"DBKEY")
#'
#' wnd.dat.xtab <- dcast(wnd.dat2,DATETIME~param,value.var = "Data.Value",mean)
#' wnd.dat.xtab$WNDS.ms <- wnd.dat.xtab$WNDS*0.44704
#' wnd.dat.xtab$u <- wnd.dat.xtab$WNDS.ms*cos(wnd.dat.xtab$WNDD*pi/180)
#' wnd.dat.xtab$v <- wnd.dat.xtab$WNDS.ms*sin(wnd.dat.xtab$WNDD*pi/180)
#'
#' wind_dat <- windrose(u,v,wnd.dat.xtab)
#' }
windrose=function(x,y,data, dtheta = 15){
  # from oce::as.windrose
  ## simplified function
  # x=wnd.dat.xtab$u
  # y=wnd.dat.xtab$v
  if(!missing(data)){
    x <- data[,deparse(substitute(x))]
    y <- data[,deparse(substitute(y))]
  }

  na_rm_val  <-  !is.na(x)&!is.na(y)
  x <-  x[na_rm_val]
  y <-  y[na_rm_val]
  xlen <- length(x)
  # pi <- atan2(1, 1) * 4;
  dt <- dtheta * pi / 180
  dt2 <- dt / 2
  R <- sqrt(x^2 + y^2)
  angle <- atan2(y, x)
  nt <- round(2 * pi / dt)
  count <- mean <- vector("numeric", nt)
  fives <- matrix(0, nt, 5)
  theta <- seq(-pi + dt2, pi - dt2, length.out = nt)

  ## binning
  ai <- 1 + floor((angle + pi) / dt)
  ai <- (ai - 1) %% nt + 1
  if (min(ai) < 1) {stop("problem setting up bins (ai<1)")}
  if (max(ai) > nt) {stop("problem setting up bins (ai>xlen)")}
  for(i in 1:nt){
    inside <- ai == i
    count[i] <- sum(inside)
    mean[i] <- mean(R[inside], na.rm = TRUE)
    fives[i, ] <- boxplot.stats(R[inside])$stats# fivenum(R[inside])
  }

  res <-  list(
    n = length(x),
    x.mean = mean(x, na.rm = TRUE), y.mean = mean(y, na.rm = TRUE),
    theta = theta * 180 / pi, # in degrees
    count = count,
    mean = mean,
    fives = fives
  )
  class(res) <- "windrose"
  return(res)
}



#' @title Plot a windrose Object
#' @description plot a windrose objet simialr to `oce::windrose.plot` but  a paired down customizable version
#'
#' @param x a `windrose-class` object
#' @param type The thing to be plotted, either the number of counts in the angle
#' interval, the mean of the values in the interval, the median of the values, or
#' a [fivenum()] representation of the values.
#'
#' @param convention String indicating whether to use meteorological convention or
#' oceanographic convention for the arrows that emanate from the centre of the
#' rose.  In meteorological convection, an arrow emanates towards the right on
#' the diagram if the wind is from the east; in oceanographic convention, such an
#' arrow indicates flow *to* the east.
#'
#' @param fill.col color of fill
#' @param bx.border.col box border color
#' @param border.col border color
#' @param box.cols cox color
#' @param lab.col label colors
#' @param lab.val label value
#' @param lab.cex label size
#' @param lab.offset label offset
#' @param max.val max value
#' @param med.lwd median line width
#' @param med.col median color
#' @param ... to pass arguments to other components of the function
#'
#' @return base plot
#' @export
#' @examples
#'\dontrun{
#' wind_dat <- windrose(u,v,wnd.dat.xtab)
#'
#' plot(wind_dat,"fivenum","oceanographic",max.val=round(max(wind_dat$fives,na.rm=T)*1.5),lab.col="black",xpd=NA)
#' axs.lab=seq(0,round(max(wind_dat$fives,na.rm=T)*1.5),length.out=3)
#' xx = seq(0,-1,length.out=length(axs.lab))
#' axis_fun(1,xx,xx,axs.lab,line=-1.2,axisLine=-0.7)
#' mtext(side=3,adj=0,"LZ40 Wind Data (m s\u207B\u00B9)",line=-1,cex=0.8,font=2)
#'
#'}

plot.windrose = function(
    x,
    type = c("count", "mean", "median", "fivenum"),
    convention = c("meteorological", "oceanographic"),
    # col <- c("red", "pink", "blue", "darkgray"),
    fill.col = "red",
    bx.border.col = "blue",
    border.col = "darkgrey",
    box.cols = c("red","pink"),
    lab.col = NULL,
    lab.val = c("S","W","N","E"),
    lab.cex = 1,
    lab.offset= 0.25,
    max.val = NULL,
    med.lwd = 2,
    med.col = "black",
    ...){
  # paired down customizable plot

  type <- match.arg(type)
  convention <- match.arg(convention)

  if(is.null(lab.col)==T){lab.col=border.col}

  # from oce::plot.windrose
  nt <- length(x$theta)
  pi <- 4.0 * atan2(1.0, 1.0)
  if (convention == "meteorological") {
    t <- x$theta * pi / 180 # in radians
  } else {
    t <- pi + x$theta * pi / 180 # in radians
  }
  dt <- t[2] - t[1]
  dt2 <- dt / 2

  pin <- par("pin")
  xlim.val <- c(-1.0, 1.0)
  ylim.val <- c(-1.0, 1.0)
  if (pin[1] > pin[2]) {
    xlim.val <- (pin[1] / pin[2]) * xlim.val
  } else {
    ylim.val <- (pin[2] / pin[1]) * ylim.val
  }

  plot(xlim.val,ylim.val,type="n",ann=F,axes=F,...)
  # Draw circle and radii
  tt <- seq(0, 2 * pi, length.out = 100)
  px <- cos(tt)
  py <- sin(tt)
  lines(px, py, col = border.col)
  for (i in 1:nt) {
    lines(c(0, cos(t[i] - dt2)), c(0, sin(t[i] - dt2)), lwd = 0.5, col = border.col)
  }
  text( 0, -1, lab.val[1], pos = 1,cex=lab.cex,col=lab.col,offset = lab.offset,...)
  text(-1,  0, lab.val[2], pos = 2,cex=lab.cex,col=lab.col,offset = lab.offset,...)
  text( 0,  1, lab.val[3], pos = 3,cex=lab.cex,col=lab.col,offset = lab.offset,...)
  text( 1,  0, lab.val[4], pos = 4,cex=lab.cex,col=lab.col,offset = lab.offset,...)

  if (type == "count") {
    max <- if(is.null(max.val)==F){max.val}else{max(x$count, na.rm = TRUE)}
    for (i in 1:nt) {
      r <- x$count[i] / max
      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
      polygon(xlist, ylist, col = fill.col, border = bx.border.col)
    }
  }

  if (type == "mean") {
    max <- if(is.null(max.val)==F){max.val}else{max(x$mean, na.rm = TRUE)}
    for (i in 1:nt) {
      r <- x$mean[i] / max
      # cat("t=", t[i], " r=", r, "\n")
      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
      polygon(xlist, ylist, col = fill.col, border = bx.border.col)
    }
  }

  if (type == "median") {
    max <- if(is.null(max.val)==F){max.val}else{max(x$fives[, 5], na.rm = TRUE)}
    for (i in 1:nt) {
      r <- x$fives[i, 3] / max
      xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
      ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
      polygon(xlist, ylist, col = fill.col, border = bx.border.col)
    }
  }

  if (type == "fivenum") {
    max <- if(is.null(max.val)==F){max.val}else{max(x$fives[, 5], na.rm = TRUE)}
    for (i in 1:nt) {
      tm <- t[i] - dt2
      tp <- t[i] + dt2
      for (j in 2:5) {
        r0 <- x$fives[i, j - 1] / max
        r <- x$fives[i, j] / max
        xlist <- c(r0 * cos(tm), r * cos(tm), r * cos(tp), r0 * cos(tp))
        ylist <- c(r0 * sin(tm), r * sin(tm), r * sin(tp), r0 * sin(tp))
        thiscol <- if(length(box.cols)==2){box.cols[c(2, 1, 1, 2)][j - 1]}else{box.cols[j - 1]}
        polygon(xlist, ylist, col = thiscol, border = border.col)
      }
      # Median in black
      r <- x$fives[i, 3] / max
      lines(c(r * cos(tm), r * cos(tp)),
            c(r * sin(tm), r * sin(tp)),
            lwd = med.lwd,col=med.col
      )
    }
  }

}


#' @title Summarize a windrose Object
#' @description Summarizes some of the data in a [windrose-class] object, like in oce package
#' @param object object A [windrose-class] object
#' @param ... Further arguments passed to or from other methods
#'
#' @return select summary stats as text
#' @export
#'
summary.windrose <- function(object, ...) {

  cat("Windrose data\n-------------\n\n")
  n <- length(object@data$theta)
  dtheta <- abs(diff(object@data$theta[1:2]))
  cat("* Have n=", n, "angles, separated by dtheta=", dtheta, "\n\n")
}
