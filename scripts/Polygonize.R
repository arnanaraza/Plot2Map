### FUNCTION TO CREATE POLYGONS FROM SUBPLOTS WITH CORNER COORDINATES IN METERS i.e. Labriere et al. 2018
### AND POSSIBLE IRREGULAR PLOTS (NON-RECTANGULAR AND NON-SQUARED)

# Function to create irregular polygons
polyIrreg <- function(coords_poly){
  d <- as.matrix(coords_poly[, c('POINT_X', 'POINT_Y')])
  ch <- chull(d)
  coords <- d[c(ch, ch[1]), ]  # closed polygon
  sp_poly <- st_polygon(list(coords))
  sp_poly_sf <- st_sfc(sp_poly, crs = st_crs(4326))
  sp_poly_df <- st_sf(geometry = sp_poly_sf, ID = 1)
  return(sp_poly_df)
}

# Function to create circular plots (as an example, you can adjust radius)
polyCirc <- function(coords_poly, radius = 10){
  center <- st_point(c(mean(coords_poly$POINT_X), mean(coords_poly$POINT_Y)))
  center_sf <- st_sfc(center, crs = st_crs(4326))
  sp_poly <- st_buffer(center_sf, dist = radius)
  sp_poly_df <- st_sf(geometry = sp_poly, ID = 1)
  return(sp_poly_df)
}
# Function to polygonize a data frame
Polygonize <- function(df, SRS){
  dat <- split(df, df$id)
  pol <- lapply(dat, function(x) polyIrreg(x))
  pol1 <- st_make_valid(st_as_sf(do.call(rbind, pol)))
  st_crs(pol1) <- st_crs(SRS)
 
   # Add PLOT_ID and SIZE_HA
  pol1$PLOT_ID <- row.names(pol1)
  pol1$SIZE_HA <- round(st_area(pol1) / 10000, 2)
  c <- st_centroid(pol1)

  pol2 <- st_transform(c, crs = 4326)
  coords <-   st_coordinates(pol2)
  
  # pol1 <- st_make_valid(pol1)# Calculate centroids
  pol1$POINT_X <- coords[, 1]
  pol1$POINT_Y <- coords[, 2]
  as.data.frame(pol1) %>% dplyr::select(-geometry)
  
}

