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

AGBtileNames <- function(pol){
  bb <- unname(bbox(pol))
  crds <- expand.grid(x=bb[1,],y=bb[2,])
  fnms <- character(6)
  match <- c('1000m', 'AGB_SD', 'aux')
  
  for(i in 1:nrow(crds)){
    
    if (grepl('2017', agbTilesDir) == TRUE){
      print('Using agb 2017 v1')
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2017-fv1.0.tif"))
      
    }else if (grepl('2010', agbTilesDir) == TRUE){
      print('Using agb 2010 v1')
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv1.0.tif"))
    }else if (grepl('2018', agbTilesDir) == TRUE){
      print('Using agb 2018 v1')
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv1.0.tif"))
    }
    
  }
  fnms0 <- unique(grep(paste(match,collapse="|"), fnms, value=T))
  fnms <- setdiff(fnms,fnms0)
  unique(fnms)
}
