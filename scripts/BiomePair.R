### FUNCTION TO GET THE CORRESPONDING ZONES AND BIOMES OF PLOT LOCATIONS USING PRE-PROCESSED SHAPEFILES

BiomePair <- function(df_defoCheck){
  
  plots0 <- df_defoCheck
  plots0 <- vect(plots0, geom=c("POINT_X", "POINT_Y"))

  li <- vect("eco_zone.shp")
  re <- vect("world_region.shp")
  
  ## convert it to 'terra'
  p <- plots0
  p$POINT_X <- df_defoCheck$POINT_X
  p$POINT_Y <- df_defoCheck$POINT_Y
  ## intersect polygons with points, keeping the information from both
  intFez0 <- terra::intersect(p, li)
  df <- terra::intersect(re, intFez0)
  
  #order first before joining
  df$ZONE <- as.character(df$SUBREGION)
  df$FAO.ecozone <- as.character(df$GEZ_TERM)
  df$GEZ <- word(df$FAO.ecozone, 1)
  df <- df[, -which(names(df) %in% c("SUBREGION", "GEZ_TERM", 'FEZ', 'ORIG_FID'))]
  
  #some cleaning
  df$GEZ <- ifelse(df$GEZ == 'Polar', 'Boreal', df$GEZ)
  df$FAO.ecozone <- ifelse(df$FAO.ecozone == 'Polar', 'Boreal coniferous forest', df$FAO.ecozone)
  
  df <- subset(df, df$GEZ != 'Water')# | df$GEZ != 'No')
  df$ZONE <- ifelse(word(df$ZONE, 1) == 'Australia', 'Australia', df$ZONE)
  df$ZONE <- ifelse(word(df$ZONE, 1) == 'South' | word(df$ZONE, 1) == 'America' , 'S.America', df$ZONE)
  df$ZONE <- ifelse(word(df$ZONE, 1) == 'Central' | word(df$ZONE, 1) == 'America' , 'C.America', df$ZONE)
  df$ZONE <- ifelse(word(df$ZONE, 2) == 'Asia' & !is.na(word(df$ZONE, 2)), 'Asia', df$ZONE)
  df$ZONE <- ifelse(word(df$ZONE, 2) == 'Africa' & !is.na(word(df$ZONE, 2)),'Africa', df$ZONE)
  df$ZONE <- ifelse(word(df$ZONE, 2) == 'Europe' & !is.na(word(df$ZONE, 2)),'Europe', df$ZONE)
  
  plt <- as.data.frame(df)
  return(plt)
  
}



