
# sample block mean mapped forest over pol
sampleTreeCover <- function(pol, thresholds, wghts=FALSE){
  TCs <- numeric()
  ras <- TCtileNames(pol)
  
  if(wghts){
    vls <- matrix(ncol=2, nrow=0)
    for(f in ras)
      if(file.exists(f))
        vls <- rbind(vls, extract(crop(raster(f),extent(pol)), pol, weights=TRUE, 
                                  normalizeWeights=FALSE) [[1]])
      # get rid of NA
      ids <- which(!is.na(vls[,1]))
      vls <- vls[ids,]
      if(length(vls) == 0) return(0)
      if(length(vls) == 2)
        vls <- matrix(vls, ncol=2)
      for (threshold in thresholds){
        tmp <- vls
        tmp[,1] <- ifelse(tmp[,1] > threshold, 1.0, 0.0)
        TCs <- c(TCs, sum(apply(tmp, 1, prod))/sum(tmp[,2]))
      }
  } else{
    vls <- numeric()
    for(f in ras)
      if(file.exists(f))
        vls <- c(vls,extract(crop(raster(f),extent(pol)),  pol) [[1]])
      for (threshold in thresholds){
        TCs <- c(TCs, mean(ifelse(vls > threshold, 1.0, 0.0), na.rm=T))
      }
  }
  TCs
}


# sample block mean mapped AGB over pol
sampleAGBmap <- function(pol, wghts=FALSE){
  ras <- AGBtileNames(pol)
  if(wghts){
    vls <- matrix(ncol=2, nrow=0)
    for(f in ras)
      if(file.exists(f))
        vls <- rbind(vls, extract(raster(f), pol, weights=TRUE, 
                                  normalizeWeights=FALSE)[[1]])
      # get rid of NA
      ids <- which(!is.na(vls[,1]))
      vls <- vls[ids,]
      if(length(vls) == 0) return(0)
      AGB <- ifelse(length(vls) == 2, vls[1], 
                    sum(apply(vls, 1, prod))/sum(vls[,2]))
  } else{
    vls <- numeric()
    for(f in ras)
      if(file.exists(f))
        vls <- c(vls, extract(raster(f), pol)[[1]])
      
      if(length(na.omit(vls)) == 0) return(0)
      AGB <- mean(vls, na.rm=T)
  }
  AGB
}