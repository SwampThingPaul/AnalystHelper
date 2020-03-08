#' GIS Function to create thessian polygons from spatial points.
#'
#' @param data spatial dataset
#' @param clip polygon (i.e. study area) to clip the thessian polygons.
#' @param plot to plot the generate thessian polygons
#' @param full.extent extend the thessian polygons to the full extent of the clip-layer.
#' @export
#' @return Returns a spatial thessian polygon

thessian_create=function(data,clip,plot=T,full.extent=T){
  #require(dismo)
  #require(sp)
  #require(raster)
  #require(rgeos)
  if(full.extent==T){bbox.da=c(bbox(clip)[1,1],bbox(clip)[1,2],bbox(clip)[2,1],bbox(clip)[2,2])}else{bbox.da=NULL}
  th=voronoi(data,ext=bbox.da)
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
