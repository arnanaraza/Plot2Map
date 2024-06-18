
SpatCor <- function (vgm=vgm, r=SD.100.ext){
  vgm0 = vgm
  #convolute VG first
  BBvar <- function(model, lat=0, rsl, dists = c(0, 1:7*10 + 2)) {
    
    # definition block discretized by pixels
    nx <- round(0.1/rsl[1], 0)
    ny <- round(0.1/rsl[2], 0)
    n <- nx * ny
    dx <- distCosine(c(0,lat), c(rsl[1], lat))/1000
    dy <- distCosine(c(0,lat), c(0, lat+rsl[2]))/1000
    x <- 1:nx * dx
    y <- ceiling(ny/2)*dy - 1:ny * dy
    pts1 <- expand.grid(x=x, y=y)  # coordinates in km
    
    # within block semivariance
    dst <- dist(pts1)
    vgs <- matrix(NA, n, n)
    vgs[lower.tri(vgs)] <- variogramLine(model, dist_vector = dst, 
                                         covariance=FALSE)$gamma
    vgs[upper.tri(vgs)] <- vgs[lower.tri(vgs)]
    diag(vgs) <- 0
    inB <-mean(vgs)
    
    # block to block semivariances; slow step that can be parallelized
    coordinates(pts1) <- ~x+y
    gamma = numeric(length(dists))
    i <- 1
    for (dst in dists){
      if(dst == 0){
        gamma[i] <- 0
      } else{
        pts2 <- pts1
        pts2@coords[,2] <-  pts2@coords[,2] + dst
        D <- spDists(pts1, pts2)
        gamma[i] <- mean(variogramLine(model, dist_vector = D, 
                                       covariance = FALSE)) - inB
      }
      i <- i + 1
    } # constructing variogram object
    n <- length(dists)
    vg <- data.frame(np = rep(n * (n-1) * 0.5, n), dist=dists, gamma=gamma)
    vg$dir.hor <- 0
    vg$dir.ver <- 0
    vg$id <- "var1"
    class(vg) <- c("gstatVariogram", "data.frame")
    vg
    }
  
  
  #get raster info
  ex <- extent(r)
  res <- xres(r)
  lat <- (ex@ymin + ex@ymax)/2
  vgm <- BBvar(model = vgm0, lat=lat, rsl = c(res, res)) #100m
  # fit model to block sample variogram; check whether nugget >= 0
  vgm <- fit.variogram(vgm, vgm0, fit.method = 6)
  vgm[1,2] <- 0
  
  blockCorr <- function(lat, rsl, model){
    nx <- round(0.1/rsl[1], 0)
    ny <- nx
    n <- nx * ny
    dx <- distCosine(c(0,lat), c(rsl[1], lat))/1000
    dy <- distCosine(c(0,lat), c(0, lat+rsl[2]))/1000
    x <- 1:nx * dx
    y <- ceiling(ny/2)*dy - 1:ny * dy
    pts <- expand.grid(x, y)  # coordinates in km
    dst <- dist(pts)
    vgs <- matrix(NA, n,n)
    vgs[lower.tri(vgs)] <- variogramLine(model, dist_vector = dst, 
                                         covariance=T)$gamma
    vgs[upper.tri(vgs)] <- vgs[lower.tri(vgs)]
    diag(vgs) <- sum(model$psill)
    vgs
  }
  
  # define the output raster
  c <- raster(xmn=ex@xmin, xmx=ex@xmax, 
              ymn=ex@ymin,ymx = ex@ymax, 
              crs=r@crs, resolution=0.1)
  
  # the dimensions below should exactly match those used for computing CM
  nx <- round(0.1/res[1], 0)
  ny <- nx
  
  #initialize block values
  nrw <- as.numeric()
  ncl <- as.numeric()
  dimBlocks <- data.frame(nrw,ncl)
  
  for(i in 1:ncell(c)){
    xto <- xFromCell(c, i)
    yto <- yFromCell(c, i)
    xmn <- xto - 0.05
    ymx <- yto + 0.05
    nrw <- max(0, floor((ex@ymax - ymx)/res[1])) + 1
    ncl <- max(0, floor((xmn - ex@xmin)/res[1])) + 1
    dimBlocks[i,] <-  c(nrw,ncl)
  }
  
  vals <- lapply(1:nrow(dimBlocks), function(x) 
    getValuesBlock(r, row=dimBlocks[x,1], col=dimBlocks[x,2], 
                   ncols=nx, nrows=ny))
  vals <- lapply(vals, function(x) {replace(x, is.na(x), 0)})
  
  CM <- blockCorr(lat, res, vgm)
  
  if(dim(CM)[1] != length(vals[[1]])){
    stop('dimensions of covariance matrix and pixels at 0.1 should match')}
  
  pixVal <- lapply(1:nrow(dimBlocks), function(x) ifelse(mean(vals[[x]]) == 0, 0, 
                                                         mean(outer(vals[[x]], vals[[x]]) * CM)))
  pixVal <- unlist(pixVal, use.names=FALSE)
  c <- setValues(c, pixVal)
  
  sqrt(c) # from variance to SD
  
}



