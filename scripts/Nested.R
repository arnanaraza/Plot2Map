### FUNCTION TO FORMAT TREE-LEVEL DATA FROM NESTED PLOTS
### CENTROID OF SUB-PLOTS WITH UNIQUE ID NEEDED AND TREE-LEVEL DATA WITH THE SUB-PLOT UNIQUE ID FOR PAIRING

Nested <- function(centroid_shp, tree_table){
  cent.sf <- st_as_sf(centroid_shp)
  cent.wgs <- spTransform(centroid_shp, CRS("+init=epsg:4326")) #to WGS84
  pol <- st_buffer(cent.sf, 5.64) #square buffer approx. 1km resolution
  
  plotTree0 <- subset(tree_table, tree_table$TREE_ALIVE == 1 & 
                        tree_table$TREE_OR_STUMP == 1) #only alive!
  
  id <-plotTree0 [,menu(names(plotTree0), title="which column is your unique Plot ID?")]
  diameter <- as.numeric(plotTree0 [,menu(names(plotTree0), 
                                          title="which column is your unique  DBH (cm)?")])
  height <-as.numeric(plotTree0 [,menu(names(plotTree0), 
                                       title="which column is your unique Tree Height (m)?")])
  genus <- readline(prompt="Enter tree genus: ")#Picea 
  species <- readline(prompt="Enter tree species: ")#sitchensis
  size <- as.numeric(readline(prompt="Enter plot size in m2: ")) #100
  fez <- NA
  gez <- NA
  year<- 2010
  
  plotTree <- data.frame(id, genus, species, diameter, size, fez, gez, year, height)
  
  #fill height for NAs
  for (i in unique(plotTree$id)){
    plotTree$height <- ifelse(plotTree$id==i & is.na(plotTree$height), 
                              mean(subset(plotTree, plotTree$id == i)$height,na.rm=T), 
                              plotTree$height)
  }
  
  cent.wgs$x <-cent.wgs@coords[,1]
  cent.wgs$y <- cent.wgs@coords[,2]
  
  xyTree <- left_join(plotTree, as.data.frame(cent.wgs), by=c('id'='POINT_GUID'))
  xyTree <- xyTree[,c('id', 'y', 'x')]
  
  list(plotTree,xyTree)
  
  
}


