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
    fnms[i] <- file.path(treeCoverDir, paste0("treecover2010_",NS, "_", WE,".tif"))
  }
  unique(fnms)
}

TCtileNames <- function(pol, year) {
  # Assuming 'bb' extraction has been corrected for 'terra' objects
  bb <- ext(pol)  # Use 'terra::ext' for SpatVector objects
  bb_vec <- c(xmin(bb), ymin(bb), xmax(bb), ymax(bb))
  
  # Generate combinations of the bounding box corners
  crds <- expand.grid(x = c(bb_vec[1], bb_vec[3]), y = c(bb_vec[2], bb_vec[4]))
  
  fnms <- character(length(crds$x))  # Initialize with the correct length based on 'crds'
  
  for(i in 1:nrow(crds)) {
    lon <- 10 * (crds$x[i] %/% 10)
    lat <- 10 * (crds$y[i] %/% 10) + 10
    LtX <- ifelse(lon < 0, "W", "E")
    LtY <- ifelse(lat < 0, "S", "N")
    WE <- paste0(sprintf('%03d', abs(lon)), LtX)
    NS <- paste0(sprintf('%02d', abs(lat)), LtY)
    
    # Adjust filename based on the year
    if(year == 2015) {
      fnms[i] <- file.path(treeCoverDir, paste0("treecover2015_", NS, "_", WE, ".tif"))
    } else if(year == 2020) {
      fnms[i] <- file.path(treeCoverDir, paste0("treecover2020_", NS, "_", WE, ".tif"))
    } else {
      # Default to 2010 if another year is provided
      fnms[i] <- file.path(treeCoverDir, paste0("treecover2010_", NS, "_", WE, ".tif"))
    }
  }
  
  return(unique(fnms))
}

AGBtileNames <- function(pol){
  bb <- unname(bbox(pol))
  crds <- expand.grid(x=bb[1,],y=bb[2,])
  fnms <- character(6)
  match <- c('1000m', 'AGB_SD', 'aux')
  
  for(i in 1:nrow(crds)){
    
    if (grepl('2017', agbTilesDir) == TRUE){
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2017-fv2.0.tif"))
      
    }else if (grepl('2010', agbTilesDir) == TRUE){
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv2.0.tif"))
    }else if (grepl('2018', agbTilesDir) == TRUE){
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2018-fv2.0.tif"))
    }else{
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv3.0.tif")) 
    }
  }
  fnms0 <- unique(grep(paste(match,collapse="|"), fnms, value=T))
  fnms <- setdiff(fnms,fnms0)
  unique(fnms)
}

SDtileNames <- function(pol){
  bb <- unname(bbox(pol))
  crds <- expand.grid(x=bb[1,],y=bb[2,])
  fnms <- character(6)
  match <- '_SD'
  
  for(i in 1:nrow(crds)){
    
    if (grepl('2017', agbTilesDir) == TRUE){
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2017-fv2.0.tif"))
      
    }else if (grepl('2010', agbTilesDir) == TRUE){
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2010-fv2.0.tif"))
    }else if (grepl('2018', agbTilesDir) == TRUE){
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2018-fv2.0.tif"))
    }else{
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(LtX, sprintf('%03d',abs(lon)))
      NS <- paste0(LtY,sprintf('%02d',abs(lat)))
      fnms[i] <- file.path(agbTilesDir, paste0(NS, WE,"_ESACCI-BIOMASS-L4-AGB_SD-MERGED-100m-2020-fv3.0.tif")) 
    }
  }
  fnms0 <- unique(grep(paste(match,collapse="|"), fnms, value=T))
  unique(fnms)
}