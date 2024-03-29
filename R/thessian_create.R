#' GIS Function to create thessian polygons from spatial points.
#'
#' @param data spatial dataset
#' @param clip polygon (i.e. study area) to clip the thessian polygons.
#' @param plot to plot the generate thessian polygons
#' @import sf
#' @export
#' @return Returns a spatial thessian polygon

thessian_create=function(data,clip,plot=T){
  voronoi_grid <- data %>%
    st_geometry()  %>%
    do.call(c, .)  %>%
    st_voronoi()  %>%
    st_collection_extract()
  st_crs(voronoi_grid)<-st_crs(data)
  voronoi_grid=st_join(st_as_sf(voronoi_grid),data,join=st_intersects,left=T)

  clipped= st_as_sf(st_intersection(st_cast(voronoi_grid), st_union(clip)))
  clipped$area<-st_area(clipped)

  if(plot==T){plot(st_geometry(clipped), col = NA)}
  return(clipped)

  ## sp code - sp and rgeos are archived.
  # if(full.extent==T){bbox.da=c(sp::bbox(clip)[1,1],sp::bbox(clip)[1,2],sp::bbox(clip)[2,1],sp::bbox(clip)[2,2])}else{bbox.da=NULL}
  # th=dismo::voronoi(data,ext=bbox.da)
  # th.z=sp::over(th,data)
  # th.z.spdf=sp::SpatialPolygonsDataFrame(th,th.z)
  # th.clp=raster::intersect(clip,th.z.spdf)
  # th.clp$area=rgeos::gArea(th.clp,byid=T)
  # if(plot==T){plot(th.clp)}
  # return(th.clp)
}


N=function(x,na.rm=FALSE){
  .Deprecated("N.obs")
}

thessian_create.v2=function(data,clip,plot=T){
  .Deprecated("thessian_create.v2")
  # thessian_create(data,clip,plot,plot=plot)
}
