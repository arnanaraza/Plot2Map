
IsLidar <- function(data.dir='D:/AGBC/data/SustainableLandscapeBrazil_v03/SLB_AGBmaps', year=2018){
                      
  raw <- list.files(data.dir)
  yrs <- 2010:2018
  if(!is.na(year)){
    yrs <- setdiff(yrs,year)
    no.match <-  c(yrs,'aux', 'csv', 'pdf', 'RData')
    no.match <- unique(grep(paste(no.match,collapse="|"), raw, value=T))
    raw1 <- setdiff(raw,no.match)
    
  }else{
    no.match <-  c('aux', 'csv', 'pdf', 'RData')
    no.match <- unique(grep(paste(no.match,collapse="|"), raw, value=T))
    raw1 <- setdiff(raw,no.match)
  }
  setwd(data.dir)
  
  r.files <- lapply(raw1, function(x) raster(x))
  rsl <- xres(raster(r.files[[1]]))
  
  if (grepl('utm',crs(r.files[[1]]), fixed = TRUE) == TRUE ){
    r.files <- lapply(1:length(r.files), function(x) 
      projectRaster(r.files[[x]], crs = newproj,method= 'bilinear'))
  }else{r.files=lapply(r.files, function(x)  aggregate(x,10,'mean'))}
   
  
  pts.list <- lapply(1:length(r.files), function(x) 
    rasterToPoints(r.files[[x]],spatial = T))
  pts.list <- lapply(pts.list, setNames, nm = 'same_name')
  pts.list <- lapply(1:length(pts.list),function(x) {pts.list[[x]]$ID <- names(r.files[[x]]);return(pts.list[[x]])})
  pts <- do.call(rbind, lapply(1:length(pts.list), function(x){
    SpatialPointsDataFrame(coords = pts.list[[x]]@coords,  data = as.data.frame(pts.list[[x]]))}))
                         
  if (grepl('AGBmaps',data.dir, fixed = TRUE) == TRUE ){
    names(pts) <- c('AGB_T_HA','ID', 'POINT_X', 'POINT_Y')
  }else  if (grepl('CVmaps',data.dir, fixed = TRUE) == TRUE ){
    names(pts) <- c('CV', 'ID','POINT_X', 'POINT_Y')
  }else{ names(pts) <- c('AGB_T_HA','ID', 'POINT_X', 'POINT_Y') }

  print(pts$ID[1])
  id.str <- c(menu(1:20, title="enter index of the first letter of PLOT ID "),
              menu(1:20, title="enter index of the last letter of PLOT ID "))
  pts$CODE =  substr(pts$ID, id.str[1], id.str[2]) 
  unique(pts$CODE)
  print(pts$ID[1])
  id.str <- c(menu(1:20, title="enter index of the first letter of YEAR"),
              menu(1:20, title="enter index of the last letter of YEAR"))
  pts$AVG_YEAR =  substr(pts$ID, id.str[1], id.str[2])
  unique(pts$AVG_YEAR)
  
  if (grepl('CVmaps',data.dir, fixed = TRUE) == TRUE ){
    pts1 <- dcast(as.data.frame(pts), POINT_X + POINT_Y  ~ AVG_YEAR, value.var="CV", fun.aggregate=mean)
    names(pts1) <- paste0('CV_',names(pts1))
    names(pts1)[1] <- 'POINT_X'
    names(pts1)[2] <- 'POINT_Y'
  }else {
    pts1 <- dcast(as.data.frame(pts), POINT_X + POINT_Y  ~ AVG_YEAR, value.var="AGB_T_HA", fun.aggregate=mean)
  }
  
  pts2 <- list() 
  for (i in 1:nrow(pts1)){
    if(sum(!is.na(pts1[i,] >3)) > 3){ #if there's a value
      pts2[[i]] <- pts1[i,]}}
  pts3= do.call(rbind, pts2)
  #pts3$size=rsl
  pts3
}

###### SLB ############

data.agb <- 'D:/AGBC/data/SustainableLandscapeBrazil_v03/SLB_AGBmaps'
data.cv <- 'E:/AGBC/data/SustainableLandscapeBrazil_v03/SLB_CVmaps'

# 2018 maps
pts2018 <- IsLidar(data.agb, 2018)
pts2018$varTot <-  (SLB(data.cv, 2018)[[1]] * pts2018$AGB_T_HA)^2
pts2018$SIZE_HA = 1
pts2018$AGB_T_HA_ORIG <- pts2018$AGB_T_HA
pts2018$sdMap <- sqrt(pts2018$varTot)
setwd('D:/AGBG/data')
write.csv(pts2018, 'slb_2018.csv',row.names = F)

# 2017 maps
pts2017 <- SLB(data.agb, 2017)
pts2017$varTot <-  (SLB(data.cv, 2017)[[1]] * pts2017$AGB_T_HA)^2
pts2017$SIZE_HA = 1
pts2017$AGB_T_HA_ORIG <- pts2017$AGB_T_HA
pts2017$sdMap <- sqrt(pts2017$varTot)
setwd('D:/AGBG/data')
write.csv(pts2017, 'slb_2017.csv',row.names = F)

# 2010 maps
pts2011 <- SLB(data.agb, 2011:2012)
pts2011$varTot <-  (SLB(data.cv, 2011:2012)[[1]] * pts2011$AGB_T_HA)^2
pts2011$SIZE_HA = 1
pts2011$AGB_T_HA_ORIG <- pts2011$AGB_T_HA
pts2011$sdMap <- sqrt(pts2011$varTot)
setwd('D:/AGBG/data')
write.csv(pts2011, 'slb_2011.csv',row.names = F)

######## US NEON #############

data.agb <- 'E:/AGBC/data/NEON_data_package_v01/NEON_AGBmaps'
data.cv <- 'E:/AGBC/data/NEON_data_package_v01/NEON_CVmaps'

# 2013-2014 maps
neon2013 <- SLB(data.agb, 2013:2014)
neon2013$varTot <-  (SLB(data.cv, 2013:2014)[[1]] * neon2013$AGB_T_HA)^2
neon2013$SIZE_HA = 1
neon2013$AGB_T_HA_ORIG <- neon2013$AGB_T_HA
neon2013$sdMap <- sqrt(neon2013$varTot)
setwd('E:/AGBG/data')
write.csv(neon2013, 'neon2013.csv',row.names = F)

# 2017 maps
neon2017 <- SLB(data.agb, 2017:2018)
neon2017$varTot <-  (SLB(data.cv, 2017:2018)[[1]] * neon2017$AGB_T_HA)^2
neon2017$SIZE_HA = 1
neon2017$AGB_T_HA_ORIG <- neon2017$AGB_T_HA
neon2017$sdMap <- sqrt(neon2017$varTot)
setwd('E:/AGBG/data')
write.csv(neon2017, 'neon2017.csv',row.names = F)

# 2018 maps
neon2018 <- SLB(data.agb, 2018:2019)
cv2018 <- SLB(data.cv, 2018:2019)
neon2018$varTot <-  (SLB(data.cv, 2018:2019)[[1]] * neon2018$AGB_T_HA)^2
neon2018$SIZE_HA = 1
neon2018$AGB_T_HA_ORIG <- neon2018$AGB_T_HA
neon2018$sdMap <- sqrt(neon2018$varTot)
setwd('E:/AGBG/data')
write.csv(neon2018, 'neon2018.csv',row.names = F)

######## AUS TERN #############
data.agb <- 'E:/AGBC/data/TERN_data_package_v01/TERN_AGBmaps'
data.cv <- 'E:/AGBC/data/TERN_data_package_v01/TERN_CVmaps'

# 2012-2014 maps
tern2014 <- SLB(data.agb, 2012:2014)
tern2014$varTot <-  (SLB(data.cv, 2012:2014)[[1]] * tern2014$AGB_T_HA)^2
tern2014$SIZE_HA = 1
tern2014$AGB_T_HA_ORIG <- tern2014$AGB_T_HA
tern2014$sdMap <- sqrt(tern2014$varTot)
setwd('E:/AGBG/data')
write.csv(tern2014, 'tern2014.csv',row.names = F)

### Merge LiDAR data
setwd('E:/AGBG/data')
lidar_2010 <- rbind( read.csv('neon2013.csv'),
                     read.csv('slb2011.csv'),
                     read.csv('tern2014.csv'))
lidar_2010$AVG_YEAR <- 2010
lidar_2010$MapName <- '10_GlobBiomass'

lidar_2017 <- rbind( read.csv('neon2017.csv'),
                     read.csv('slb2017.csv'))
lidar_2017$AVG_YEAR <- 2017
lidar_2017$MapName <- '17_CCIBiomass'

lidar_2018 <- rbind( read.csv('neon2018.csv'),
                     read.csv('slb2018.csv'))    
lidar_2018$AVG_YEAR <- 2018
lidar_2018$MapName <- '18_CCIBiomass'

lidar_plt <- rbind(lidar_2010,lidar_2017,lidar_2018)
plt <- lidar_plt
coordinates(plt) <- ~POINT_X+POINT_Y
lidar_plt$slope <- extract(raster('D:/AGBG/data/slope_1km.tif'),plt)
lidar_plt$aspect <- extract(raster('D:/AGBG/data/aspect_1km.tif'),plt)
lidar_plt$tc <- extract(raster('D:/AGBG/data/TC2010_1km.tif'),plt)
write.csv(lidar_plt, 'lidar_plt.csv',row.names=F)


