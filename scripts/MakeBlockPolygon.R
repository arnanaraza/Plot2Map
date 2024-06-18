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


# make Polygon containing x, y and aligning to AGB map pixel  
# or larger cell over which AGB is aggregated
MakeBlockPolygon <- function(x, y, size) {
  # Calculate the lower left corner of the cell
  xll <- size * (x %/% size)
  yll <- size * (y %/% size)
  
  # Define the vertices of the polygon
  vertices <- rbind(
    c(xll, yll),
    c(xll + size, yll),
    c(xll + size, yll + size),
    c(xll, yll + size),
    c(xll, yll) # Close the polygon by repeating the first point
  )
  
  # Use terra to create a SpatVector for the polygon
  # Create a matrix with coordinates for lines to form a polygon
  mat <- rbind(vertices, vertices[1,]) # Ensure the polygon is closed by repeating the first vertex
  
  # Create a SpatVector from the coordinates
  pol <- vect(mat, type = "polygons", crs = "+proj=longlat +datum=WGS84")
  
  return(pol)
}

