#' GIS Function to create thessian polygons from spatial points.
#'
#' @param data spatial dataset
#' @param clip polygon (i.e. study area) to clip the thessian polygons.
#' @param plot to plot the generate thessian polygons
#' @param full.extent extend the thessian polygons to the full extent of the clip-layer.
#' @export
#' @return Returns a spatial thessian polygon

thessian_create=function(data,clip,plot=T,full.extent=T){

  if(full.extent==T){bbox.da=c(sp::bbox(clip)[1,1],sp::bbox(clip)[1,2],sp::bbox(clip)[2,1],sp::bbox(clip)[2,2])}else{bbox.da=NULL}
  th=dismo::voronoi(data,ext=bbox.da)
  th.z=sp::over(th,data)
  th.z.spdf=sp::SpatialPolygonsDataFrame(th,th.z)
  th.clp=raster::intersect(clip,th.z.spdf)
  th.clp$area=rgeos::gArea(th.clp,byid=T)
  if(plot==T){plot(th.clp)}
  return(th.clp)
}

thessian_create.v2=function(data,clip,plot=T,full.extent=T){
  thessian_create(data,clip,plot,full.extent)
}
