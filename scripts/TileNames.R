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
    lon <- 2*(crds[i,1]%/%2)
    lat <- 2*(crds[i,2]%/%2) + 2
    LtX <- ifelse(lon < 0, "W", "E")
    LtY <- ifelse(lat < 0, "S", "N")
    WE <- paste0(LtX, sprintf('%03d',abs(lon)))
    NS <- paste0(LtY, sprintf('%02d',abs(lat))) 
    fnms[i] <- file.path(agbTilesDir, paste0(NS,WE,"_agb_100m.tif"))
  }
  unique(fnms)
}
