### FUNCTION TO CREATE POLYGONS FROM SUBPLOTS WITH CORNER COORDINATES IN METERS i.e. Labriere et al. 2018
### AND POSSIBLE IRREGULAR PLOTS (NON-RECTANGULAR AND NON-SQUARED)

#Sample irregular polygon 
polyIrreg <- function(coords_poly){
  d <- as.matrix(coords_poly[,c('POINT_X','POINT_Y')])
  ch <- chull(d)
  coords <- d[c(ch, ch[1]), ]  # closed polygon
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
  return(sp_poly_df)
}

#Sample circular plots
polyCirc <- function(shp){
  d <- as.matrix(coords_poly[,c('POINT_X','POINT_Y')])
  ch <- chull(d)
  coords <- d[c(ch, ch[1]), ]  # closed polygon
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID=1)))
  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data=data.frame(ID=1))
  return(sp_poly_df)
}


#TropiSar/AfriSar dataset 
Polygonize <- function(df=plotsPoly, SRS='+init=epsg:32622'){
  dat <- split(df, df$id)
  pol <- lapply(dat, function(x) polyIrreg(x))
  pol1 <- list(pol, makeUniqueIDs = T) %>%  #non-unique IDs
    flatten() %>% 
    do.call(rbind, .)
  proj4string(pol1) <- SRS
  pol1 <- spTransform(pol1, CRS("+init=epsg:4326")) #to WGS84
  pol1$PLOT_ID <- ldply(lapply(dat, function(x) unique(x$id)), data.frame)[[1]]
  pol1$SIZE_HA <- round(raster::area(pol1)/10000,2)
  c <- gCentroid(pol1,byid=TRUE)
  pol1$POINT_X <- c@coords[,1]
  pol1$POINT_Y <- c@coords[,2]
  return(pol1)
}

