### IF LIDAR-BASED MAPS ARE TO BE USED AS REFERENCE DATA, THIS FUNCTION WILL CONVERT EACH MAP PIXEL
### TO POINTS AND OBTAIN  NECESSARY INFO SUCH AS YEAR AND PLOT ID. THE MAP SD LAYERS WILL BE INCLUDED

RefLidar <- function(lidar.dir='D:/AGBC/data/SustainableLandscapeBrazil_v03/SLB_AGBmaps',year=2018){
  newproj <- "+proj=longlat +datum=WGS84"           
  raw <- list.files(lidar.dir)
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
  setwd(lidar.dir)
  
  r.files <- lapply(raw1, function(x) {
    r <- raster(x)
    names(r) <- x
    names(r[[1]]) <- x
    r})
  
  
  if (grepl('utm', crs(r.files[[1]]), fixed = TRUE) |
      grepl('meters', crs(r.files[[1]]), fixed = TRUE) |
      grepl('metre', crs(r.files[[1]]), fixed = TRUE) |
      grepl('UTM', crs(r.files[[1]]), fixed = TRUE) |
      grepl('zone', crs(r.files[[1]]), fixed = TRUE) |
      grepl('NAD', crs(r.files[[1]]), fixed = TRUE)) {

    r.files <- lapply(1:length(r.files), function(x) 
      projectRaster(r.files[[x]], crs = newproj,method= 'bilinear'))
    
    
  }
  else{
  #  r.files=lapply(r.files, function(x) aggregate(x,10,'mean')) no need to aggregate!
    r.files <- r.files
  }
  
  ha <- xres(r.files[[1]]) * 1000
  
  #pixels to points
  pts.list <- lapply(1:length(r.files), function(x) 
    rasterToPoints(r.files[[x]],spatial = T))
  pts.list <- lapply(pts.list, setNames, nm = 'same_name')
  pts.list <- lapply(1:length(pts.list),function(x) {pts.list[[x]]$ID <- names(r.files[[x]]);return(pts.list[[x]])})
  pts <- do.call(rbind, lapply(1:length(pts.list), function(x){
    SpatialPointsDataFrame(coords = pts.list[[x]]@coords,data = as.data.frame(pts.list[[x]]))}))
                           
  if (grepl('AGBmaps',lidar.dir, fixed = TRUE) == TRUE ){
    names(pts) <- c('AGB_T_HA','ID', 'POINT_X', 'POINT_Y')
    }else  if (grepl('CVmaps',lidar.dir, fixed = TRUE) == TRUE ){
      names(pts) <- c('CV', 'ID','POINT_X', 'POINT_Y')
    }else{ names(pts) <- c('AGB_T_HA','ID', 'POINT_X', 'POINT_Y') }

  print(pts$ID[1])
  id.str <- c(menu(1:20, title="enter index of the first letter of PLOT ID "),
              menu(1:20, title="enter index of the last letter of PLOT ID "))
  pts$PLOT_ID =  substr(pts$ID, id.str[1], id.str[2]) 

  print(pts$ID[1])
  id.str <- c(menu(1:30, title="enter index of the first letter of YEAR"),
              menu(1:30, title="enter index of the last letter of YEAR"))
  pts$AVG_YEAR =  substr(pts$ID, id.str[1], id.str[2])

  setwd(dataDir)
  
  if (grepl('AGBmaps',lidar.dir, fixed = TRUE) == TRUE ){
    pts <- pts[,c('PLOT_ID', 'POINT_X', 'POINT_Y', 'AGB_T_HA', 'AVG_YEAR')]
  }else  if (grepl('CVmaps',lidar.dir, fixed = TRUE) == TRUE ){
    pts <- pts[,c('PLOT_ID', 'POINT_X', 'POINT_Y', 'CV', 'AVG_YEAR')]
  }
    
  pts$SIZE_HA <- ha
  pts = data.frame(pts)
  pts[,-c(length(pts)-2,length(pts)-1,length(pts))]
}
