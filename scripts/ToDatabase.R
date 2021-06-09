### FUNCTION TO INTEGRATE YOUR PLOT DATASET TO THE WUR PLOT DATABASE

ToDatabase <- function(plt, CODE='AFR_GHA',mapYear){
  
  if (mapYear == 18){
    plt$MapName <- '18_CCIBiomass'
  } else if (mapYear == 17){
    plt$MapName <- '17_CCIBiomass'}
  else{plt$MapName <- '10_GlobBiomass'}
  
  cols <-c('CODE', 'AGB_T_HA', 'SIZE_HA','GEZ', 'AVG_YEAR','ZONE','POINT_X','POINT_Y','sdTree','sdSE','AGB_T_HA_ORIG',
           'sdGrowth','varTot','MapName' ,'VER', 'sdMap','BIO', 'REALM','OPEN','INVENTORY','TIER')
  
  
  plt$VER <- 2
  plt$OPEN <- 1 #0-open 
  plt$INVENTORY <- 'local'
  
  plt$TIER <- ifelse(plt$SIZE_HA < 0.6, 'tier1', NA)
  plt$TIER <- ifelse(plt$SIZE_HA < 3 & plt$SIZE_HA >= 0.6,
                                'tier2', plt$TIER )
  plt$TIER <- ifelse(plt$SIZE_HA >= 3, 'tier3', plt$TIER )
  
  plt0 <- plt
  coordinates(plt) <- ~POINT_X+POINT_Y
  setwd(dataDir)
  biome <-raster('Ecoregions2017_biome.tif')
  realm <- raster('Ecoregions2017_realm.tif')
  bioID <- read.csv('biome_id.csv')
  realmID <- read.csv('realm_id.csv')
  vlsBiome <- extract(biome, plt)
  vlsRealm <- extract(realm, plt)
  plt0$BIO1 <- vlsBiome
  plt0$REALM1 <- vlsRealm
  plt <- left_join(as.data.frame(plt0),bioID, by=c('BIO1'='ID'))
  plt <- left_join(plt,realmID, by=c('REALM1'='ID'))
  plt$BIO <- plt[[length(plt)-1]]
  plt$REALM <- plt[[length(plt)]]
  plt$sdMap <- NA
  plt$CODE <- CODE
  
  if(nrow(plt) > 1000){
    plt$INVENTORY <- 'regional'
  }else{    plt$INVENTORY <- 'local'
}
  
  plt <- plt[,cols]
  plt
}

