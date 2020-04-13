### FUNCTION TO GET THE CORRESPONDING ZONES AND BIOMES OF PLOT LOCATIONS USING PRE-PROCESSED SHAPEFILES

BiomePair <- function(df){
  plots0 <- df
  SRS <- CRS("+init=epsg:4326")
  coordinates(plots0) <- ~POINT_X+POINT_Y
  proj4string(plots0) <- SRS
  fez <-readOGR(dsn=dataDir, layer = "eco_zone")
  zone <-readOGR(dsn=dataDir, layer = "world_region")
  
  ## convert it to 'sf'
  p <- st_as_sf(plots0)
  li <- st_as_sf(fez)
  re  <- st_as_sf(zone)
  
  ## intersect polygons with points, keeping the information from both
  intZone <- st_intersection(p,re)
  intFez <- st_intersection(p,li)
  
  ## omit some non-overlap for now (shouldn't be the case!)
  intZone <- st_join(p, intZone,st_intersects, left=T)
  intZone <- intZone[!duplicated(intZone$geometry), ]

  intFez <- st_join(p, intFez,st_intersects, left=T)
  intFez <- intFez[!duplicated(intFez$geometry), ]
  
  #order first before joining
  intZone1 <- intZone[order(intZone$PLOT_ID.x), ]
  intFez1 <- intFez[order(intFez$PLOT_ID.x), ]
  plots0 <- plots0[order(plots0$PLOT_ID), ]
  plots0$ZONE <- as.character(intZone1$SUBREGION)
  plots0$FAO.ecozone <- intFez1$GEZ_TERM
  plots0$GEZ <- word(plots0$FAO.ecozone, 1)
  
  #some cleaning
  plots0 <- subset(plots0, plots0$GEZ != 'Water')# | plots0$GEZ != 'No')
  plots0$ZONE <- ifelse(word(plots0$ZONE, 1) == 'Australia', 'Australia', plots0$ZONE)
  plots0$ZONE <- ifelse(word(plots0$ZONE, 1) == 'South' | word(plots0$ZONE, 1) == 'America' , 'S.America', plots0$ZONE)
  plots0$ZONE <- ifelse(word(plots0$ZONE, 1) == 'Central' | word(plots0$ZONE, 1) == 'America' , 'C.America', plots0$ZONE)
  
  plots0$ZONE <- ifelse(word(plots0$ZONE, 2) == 'Asia' & !is.na(word(plots0$ZONE, 2)), 
                        'Asia', plots0$ZONE)
  plots0$ZONE <- ifelse(word(plots0$ZONE, 2) == 'Africa' & !is.na(word(plots0$ZONE, 2)), 
                        'Africa', plots0$ZONE)
  plots0$ZONE <- ifelse(word(plots0$ZONE, 2) == 'Europe' & !is.na(word(plots0$ZONE, 2)), 
                        'Europe', plots0$ZONE)
  
  plt <- as.data.frame(plots0)
  #plt <- plt[order(plt$POINT_Y), ]
  #plt$ZONE <- if(is.na(plt$ZONE)) plt$ZONE else plt$ZONE
  return(plt)
  
}

