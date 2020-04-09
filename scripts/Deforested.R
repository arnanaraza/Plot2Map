### SCRIPT TO REMOVE DEFORESTED PLOTS BY OVERLAYING PLOTS TO FOREST LOSS DATA FROM GLOBAL FOREST CHANGE 
### Hansen et al., 2013 


Deforested <- function(plt=plt, fdir = flFolder, map_year=10){
  
defo <- c()  
  for (p in 1:nrow(plt)){
    
    #make a square polygon from plot size
    xy <- SpatialPoints(plt[p,c('POINT_X', 'POINT_Y')])
    ch <- chull(plt[p,c('POINT_X', 'POINT_Y')]) #main plot ID needed
    ww <- ifelse(!(is.na(plt[p,]$SIZE_HA)),  
                 (sqrt(plt[p,]$SIZE_HA*10000) *0.00001)/2, 0.0002) #mean of plots for NAs
    ww <- ifelse(ww < 0, abs(ww), ww)
    pol <- gBuffer(xy, width=ww, quadsegs=1, capStyle="SQUARE")
    proj4string(pol) <- SRS
    res <- pol@bbox[4] - pol@bbox[2]
    print(paste('processing:',round((res / 0.00001 * res / 0.00001 ) /10000,2), 'ha'))#checker
    
    #downloads respective forest loss tile/s from squared plots
    bb <- unname(bbox(pol))
    crds <- expand.grid(x=bb[1,],y=bb[2,])
    fnms <- character(4)
    
    dir.create(file.path(fdir))
    setwd(file.path(fdir))
    gfcTile <- calc_gfc_tiles(pol)
    download_tiles(gfcTile, fdir, images = "lossyear", dataset='GFC-2018-v1.6')
    
    #get overlapping tile/s (up to 4 possible tiles)
    for(i in 1:nrow(crds)){
      
      lon <- 10*(crds[i,1]%/%10)
      lat <- 10*(crds[i,2]%/%10) + 10
      LtX <- ifelse(lon < 0, "W", "E")
      LtY <- ifelse(lat < 0, "S", "N")
      WE <- paste0(sprintf('%03d',abs(lon)), LtX)
      NS <- paste0(sprintf('%02d',abs(lat)), LtY)
      
      fnms[i] <- file.path(fdir, paste0('Hansen_GFC-2018-v1.6_lossyear_',NS, "_", WE,".tif"))
      
      vls <- numeric()
      for(f in fnms){
        if(file.exists(f))
          vls <- c(vls, extract(raster(f), pol)[[1]])
        }
    }
    vls <- if(length(vls[vls==0]) > length(vls[vls>0])) vls*0 else(c(vls)) 
      #if there is more non-deforested (zeros)
    
    defo[p] <- mean(vls[vls>0], na.rm=T) # exact 0 means no deforestation, will return NaN
  }

plt$defo <- defo
print(plt)
defPlt <- subset(plt, plt$defo > 0 ) #if there is deforestation
defPlt <- subset(defPlt, defPlt$defo <= map_year)#if plot is older or equal to map year
netPlt <- setdiff(plt, defPlt)[,-length(defPlt)] #removes defo checker column
return(netPlt) #returns non-deforested plots 
}

