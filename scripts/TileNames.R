# Up to four treecover (TC) tile names covered by pol
TCtileNames <- function(pol){
  bb <- unname(bbox(pol))
  crds <- expand.grid(x=bb[1,],y=bb[2,])
  fnms <- character(4)
  for(i in 1:nrow(crds)){
    lon <- 10*(crds[i,1]%/%10)
    lat <- 10*(crds[i,2]%/%10) + 10
    LtX <- ifelse(lon < 0, "W", "E")
    LtY <- ifelse(lat < 0, "S", "N")
    WE <- paste0(sprintf('%03d',abs(lon)), LtX)
    NS <- paste0(sprintf('%02d',abs(lat)), LtY)
    fnms[i] <- file.path(treeCoverDir, paste0(NS, "_", WE,"_treecover2010_v3.tif"))
  }
  unique(fnms)
}


# Up to four AGB tile names covered by pol
AGBtileNames <- function(pol){
  bb <- unname(bbox(pol))
  crds <- expand.grid(x=bb[1,],y=bb[2,])
  fnms <- character(4)
  
  for(i in 1:nrow(crds)){
    
    if (grepl('Baccini', agbTilesDir) == TRUE){
      checkAGB <-print('Using Baccini tiles')
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(sprintf('%03d',abs(lon)), LtX)
      NS <- paste0(sprintf('%02d',abs(lat)), LtY)
      fnms[i] <- file.path(agbTilesDir, paste0(NS, "_", WE,"_t_aboveground_biomass_ha_2000.tif"))
    }else if (grepl('Avitabile', agbTilesDir) == TRUE){
      print('Using Avitabile tiles')
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(sprintf('%03d',abs(lon)), LtX)
      NS <- paste0(sprintf('%02d',abs(lat)), LtY)
      fnms[i] <- file.path(agbTilesDir, paste0(NS, "_", WE,"_merge.tif"))
    }else if (grepl('CCIBiomass', agbTilesDir) == TRUE){
      print('Using CCIBiomass 2017 tiles')
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_agb.tif"))
    }
    else if (grepl('GlobBiomass', agbTilesDir) == TRUE){
      lon <- 40*(crds[i,1]%/%40) 
      lon1 <- ifelse(crds[i,1] > lon, 40*(crds[i,1]%/%40) - 20, 40*(crds[i,1]%/%40) + 20)
      lon2 <- ifelse(lon1 - crds[i,1]  < 20 & lon1 - crds[i,1]  > 0 & lon1 > 0, lon1 - 40, lon1) #if East (positive)
      lon <- ifelse(lon1 - crds[i,1]  < -40, lon1 + 40, lon2) #if West (negative)
      lat <- 40*(crds[i,2]%/%40) + 40
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_agb.tif"))
    }
  
  }
  unique(fnms)
}
