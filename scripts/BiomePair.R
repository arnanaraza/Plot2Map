### FUNCTION TO GET THE CORRESPONDING ZONES AND BIOMES OF PLOT LOCATIONS USING PRE-PROCESSED SHAPEFILES

BiomePair <- function(df){
  plots0 <- df
  SRS <- CRS("+init=epsg:4326")
  coordinates(plots0) <- ~POINT_X+POINT_Y
  proj4string(plots0) <- SRS
  fez <-readOGR(dsn=dataDir, layer = "eco_zone")
  zone <-readOGR(dsn=dataDir, layer = "world_region")
  
  ## convert it to 'sf'
  sf::sf_use_s2(FALSE)
  p <- sf::st_as_sf(plots0)
  li <- sf::st_as_sf(fez)
  re  <- sf::st_as_sf(zone)
  
  ## intersect polygons with points, keeping the information from both
  intFez0 <- sf::st_intersection(p,li)
  df <- sf::st_intersection(re,intFez0)

  
  #order first before joining
  df$ZONE <- as.character(df$SUBREGION)
  df$FAO.ecozone <-  as.character(df$GEZ_TERM)
  df$GEZ <- word(df$FAO.ecozone, 1)
  df <- df[ , -which(names(df) %in% c("SUBREGION","GEZ_TERM",'FEZ','ORIG_FID'))]
  
  #some cleaning
  df$GEZ <- ifelse(df$GEZ == 'Polar', 'Boreal', df$GEZ)
  df$FAO.ecozone <- ifelse(df$FAO.ecozone == 'Polar', 'Boreal coniferous forest', df$FAO.ecozone)
  
  df <- subset(df, df$GEZ != 'Water')# | df$GEZ != 'No')
  df$ZONE <- ifelse(word(df$ZONE, 1) == 'Australia', 'Australia', df$ZONE)
  df$ZONE <- ifelse(word(df$ZONE, 1) == 'South' | word(df$ZONE, 1) == 'America' , 'S.America', df$ZONE)
  df$ZONE <- ifelse(word(df$ZONE, 1) == 'Central' | word(df$ZONE, 1) == 'America' , 'C.America', df$ZONE)
  
  df$ZONE <- ifelse(word(df$ZONE, 2) == 'Asia' & !is.na(word(df$ZONE, 2)), 
                        'Asia', df$ZONE)
  df$ZONE <- ifelse(word(df$ZONE, 2) == 'Africa' & !is.na(word(df$ZONE, 2)), 
                        'Africa', df$ZONE)
  df$ZONE <- ifelse(word(df$ZONE, 2) == 'Europe' & !is.na(word(df$ZONE, 2)), 
                        'Europe', df$ZONE)
  
  plt <- as.data.frame(df)
  plt$POINT_X <- st_coordinates(plt$geometry)[,1]
  plt$POINT_Y <- st_coordinates(plt$geometry)[,2]
  plt <- plt[ , -which(names(plt) %in% "geometry")]
  
  return(plt)
  
}

