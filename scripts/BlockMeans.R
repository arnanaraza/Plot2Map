
# sample block mean mapped forest over pol
sampleTreeCover <- function(pol, thresholds, wghts=FALSE, fmask=NA){ #add a condition where own forest mask of country is used
  TCs <- numeric()
  if(class(fmask)!='RasterLayer'){
    ras <- TCtileNames(pol)
    
    if(wghts){
      vls <- matrix(ncol=2, nrow=0)
      for(f in ras)
        if(file.exists(f))
          vls <- rbind(vls, extract(raster(f), pol, weights=TRUE,
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
          vls <- c(vls,extract(raster(f),  pol) [[1]])
        for (threshold in thresholds){
          TCs <- c(TCs, mean(ifelse(vls > threshold, 1.0, 0.0), na.rm=T))
        }
    }
  }else{
    #OWN FOREST MASK!
    fmask <- fmask
    vls <-extract(fmask,  pol)[[1]]
    vls <- ifelse(is.na(vls), 0,1)
    TCs <- mean(vls)
  }
  
  TCs
}


# sample block mean mapped AGB over pol
sampleAGBmap <- function(pol, wghts=FALSE, own=TRUE){
  
  if (own==T){
    AGBown [AGBown==0] <- NA
    
    vls <- matrix(ncol=2, nrow=0)
    vls <- rbind(vls, extract(AGBown, pol, weights=TRUE, 
                              normalizeWeights=FALSE)[[1]])
    # get rid of NA
    ids <- which(!is.na(vls[,1]))
    vls <- vls[ids,]
    if(length(vls) == 0) return(0)
    AGB <- ifelse(length(vls) == 2, vls[1], 
                  sum(apply(vls, 1, prod))/sum(vls[,2]))

  } else{
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
  }
  AGB
}




#######

# Function to sample block mean mapped AGB over a polygon
sampleAGBmap <- function(pol, wghts = FALSE, own = TRUE) {
  AGB <- NA
  
  if (own == TRUE) {
    AGBown[AGBown == 0] <- NA
    vls <- matrix(ncol = 2, nrow = 0)
    
    tryCatch({
      extracted_vals <- extract(AGBown, st_as_sf(pol), weights = TRUE, normalizeWeights = FALSE)
      if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
        vls <- rbind(vls, extracted_vals[[2]])
      }
    }, error = function(e) {
      message("Error extracting values from AGBown: ", e$message)
    })
    
    if (nrow(vls) > 0) {
      ids <- which(!is.na(vls[,1]))
      vls <- vls[ids,]
      if (length(vls) == 2) {
        AGB <- vls[1]
      } else if (nrow(vls) > 0) {
        AGB <- sum(apply(vls, 1, prod)) / sum(vls[,2])
      }
    }
  } else {
    ras <- AGBtileNames(pol)
    
    if (wghts) {
      vls <- matrix(ncol = 2, nrow = 0)
      for (f in ras) {
        if (file.exists(f)) {
          tryCatch({
            raster_obj <- rast(f)
            extracted_vals <- extract(raster_obj, st_as_sf(pol), weights = TRUE, normalizeWeights = FALSE)
            if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
              vls <- rbind(vls, extracted_vals[[2]])
            }
          }, error = function(e) {
            message("Error extracting values from ", f, ": ", e$message)
          })
        }
      }
      
      if (nrow(vls) > 0) {
        ids <- which(!is.na(vls[,1]))
        vls <- vls[ids,]
        if (length(vls) == 2) {
          AGB <- vls[1]
        } else if (nrow(vls) > 0) {
          AGB <- sum(apply(vls, 1, prod)) / sum(vls[,2])
        }
      }
    } else {
      vls <- numeric()
      for (f in ras) {
        if (file.exists(f)) {
          tryCatch({
            raster_obj <- rast(f)
            extracted_vals <- extract(raster_obj, st_as_sf(pol))
            if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
              vls <- c(vls, extracted_vals[[2]])
            }
          }, error = function(e) {
            message("Error extracting values from ", f, ": ", e$message)
          })
        }
      }
      
      if (length(na.omit(vls)) > 0) {
        AGB <- mean(vls, na.rm = TRUE)
      }
    }
  }
  
  return(AGB[1])
}

# Function to sample block mean mapped forest over a polygon
sampleTreeCover <- function(pol, thresholds, wghts = FALSE, fmask = NA) {
  TCs <- numeric()
  
  if (!inherits(fmask, 'SpatRaster')) {
    ras <- TCtileNames(pol)
    pol <- st_as_sf(pol)
    
    if (wghts) {
      vls <- matrix(ncol = 2, nrow = 0)
      for (f in ras) {
        if (file.exists(f)) {
          tryCatch({
            raster_obj <- rast(f)
            extracted_vals <- extract(raster_obj, pol, weights = TRUE, normalizeWeights = FALSE)
            if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
              vls <- rbind(vls, extracted_vals[[2]])
            }
          }, error = function(e) {
            message("Error extracting values from ", f, ": ", e$message)
          })
        }
      }
      
      if (nrow(vls) > 0) {
        ids <- which(!is.na(vls[,1]))
        vls <- vls[ids,]
        if (length(vls) == 2) vls <- matrix(vls, ncol = 2)
        
        for (threshold in thresholds) {
          tmp <- vls
          tmp[,1] <- ifelse(tmp[,1] > threshold, 1.0, 0.0)
          if (sum(tmp[,2]) > 0) {
            TCs <- c(TCs, sum(apply(tmp, 1, prod)) / sum(tmp[,2]))
          }
        }
      }
    } else {
      vls <- numeric()
      for (f in ras) {
        if (file.exists(f)) {
          tryCatch({
            raster_obj <- rast(f)
            extracted_vals <- extract(raster_obj, pol)
            if (!is.null(extracted_vals) && length(extracted_vals[[2]]) > 0) {
              vls <- c(vls, extracted_vals[[2]])
            }
          }, error = function(e) {
            message("Error extracting values from ", f, ": ", e$message)
          })
        }
      }
      
      if (length(vls) > 0) {
        for (threshold in thresholds) {
          TCs <- c(TCs, mean(ifelse(vls > threshold, 1.0, 0.0), na.rm = TRUE))
        }
      }
    }
  } else {
    vls <- extract(fmask, pol)[[1]]
    if (!is.null(vls)) {
      vls <- ifelse(vls != 1, 0, 1)
      TCs <- mean(vls)
    }
  }
  
  return(TCs[1])
}


