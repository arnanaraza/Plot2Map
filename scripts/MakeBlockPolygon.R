# make Polygon containing x, y and aligning to AGB map pixel  
# or larger cell over which AGB is aggregated

MakeBlockPolygon <- function(x, y, size){
  xll <- size * x %/% size
  yll <- size * y %/% size
  pol0 <- Polygon(cbind(c(xll, xll+size, xll+size, xll, xll),
                        c(yll, yll, yll+size, yll+size, yll)))
  pol1 <- Polygons(list(pol0), "pol")
  return(SpatialPolygons(list(pol1), proj4string=SRS))
}

