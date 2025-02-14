## Preliminaries
pacman::p_load(terra,tidyr,caret,ranger,plyr,dplyr,foreach,doParallel,Metrics,ggplot2,
               plotrix,raster,data.table,leaflet,sf,mlbench,mapview,stringr,rgeos,pdp,
               gstat,geosphere,corrplot,ggalt,dismo,hydroGOF,stringi)

# Global variables, files and folders
plotsFolder <-'/Users/renflores/Documents/GFC/Plot2Map/results'
dataDir <- '/Users/renflores/Documents/GFC/Plot2Map/results'
setwd(dataDir)

###### PREPROCESSING ---------------------------------------------------------------------

#add brazil and NEON
bra10 <- read.csv("/Users/renflores/Documents/GFC/Plot2Map/results/ValidationData_Brazil_NFI2020.csv") #from plot2map preprocessing

neon10 <- read.csv("/Users/renflores/Documents/GFC/Plot2Mapp/results/ValidationData_NEON2020.csv") #from plot2map preprocessing

# #add newest Oct 2024
# jap10 <- read.csv("C:/PlotToMap - FOR PVIR/results/ValidationData_JAP_NFI_2010.csv") #from plot2map preprocessing
# names(jap10)[1] <- 'CODE'
# jap10$CODE <- 'ASI_JAP'
# jap10$INVENTORY <- 'national'
# jap10$MapName <- '10_GlobBiomass'

# ita10 <- read.csv("C:/PlotToMap - FOR PVIR/results/ValidationData_ITA_NFI_2010.csv")
# names(ita10)[1] <- 'CODE'
# ita10$CODE <- 'EU_ITAL'
# ita10$INVENTORY <- 'national'
# ita10$MapName <- '10_GlobBiomass'

# ire10 <- read.csv("C:/PlotToMap - FOR PVIR/results/ValidationData_IRE_NFI_2010.csv")
# names(ire10)[1] <- 'CODE'
# ire10$CODE <- 'EU_IRE'
# ire10$INVENTORY <- 'national'
# ire10$MapName <- '10_GlobBiomass'

# czh10 <- read.csv("C:/PlotToMap - FOR PVIR/results/ValidationData_CZH_NFI_2010.csv")
# names(czh10)[1] <- 'CODE'
# czh10$CODE <- 'EU_CZH'
# czh10$INVENTORY <- 'national'
# czh10$MapName <- '10_GlobBiomass'

# allPlotsNew <- rbind(jap10,ita10,czh10,ire10)
allPlotsNew <- rbind(bra10, neon10)

remove <- c('FEZ', 'FAO.ecozone',  "RS_HA","ratio" )
allPlotsNew$VER <- 6
allPlotsNew <- allPlotsNew[ , !(names(allPlotsNew) %in% remove)]
allPlotsNew$sdMap <- NA #######should have a value to be consistent with PVIR1-2 e.g. @1km SD layers extract

#add bio-realms
plots0 <- allPlotsNew
coordinates(plots0) <- ~POINT_X+POINT_Y
biome <-raster('/Users/renflores/Documents/GFC/Plot2Map/data/Ecoregions2017_biome.tif')
realm <- raster('/Users/renflores/Documents/GFC/Plot2Map/data/Ecoregions2017_realm.tif')
bioID <- read.csv('/Users/renflores/Documents/GFC/Plot2Map/data/biome_id.csv')
realmID <- read.csv('/Users/renflores/Documents/GFC/Plot2Map/data/realm_id.csv')
vlsBiome <- raster::extract(biome, plots0)
vlsRealm <- raster::extract(realm, plots0)
plots0$BIO <- vlsBiome
plots0$REALM <- vlsRealm
plots0$BIO <- ifelse(plots0$BIO == 0, plots0$BIO[which(plots0$BIO == 0) + 1], plots0$BIO)
plots0$BIO <- ifelse(plots0$BIO == 0, plots0$BIO[which(plots0$BIO == 0) + 1], plots0$BIO)
plots0$REALM <- ifelse(plots0$REALM == 0, plots0$REALM[which(plots0$REALM == 0) + 1], plots0$REALM)
plots0$REALM <- ifelse(plots0$REALM == 0, plots0$REALM[which(plots0$REALM == 0) + 1], plots0$REALM)
plots1 <- left_join(as.data.frame(plots0),bioID, by=c('BIO'='ID'))
plots1 <- left_join(plots1,realmID, by=c('REALM'='ID'))
plots1$BIO <- plots1[[length(plots1)-1]]
plots1$REALM <- plots1[[length(plots1)]]
allPlotsNew <- plots1[,-c(length(plots1)-1, length(plots1))]
allPlotsNew$OPEN <- 0 #open = 0
allPlotsNew$TIER <- NA #open = 0
allPlotsNew <- allPlotsNew %>%
  dplyr::select(CODE, AGB_T_HA, SIZE_HA, GEZ, AVG_YEAR, ZONE, POINT_X, POINT_Y,
                sdTree, sdSE, AGB_T_HA_ORIG, sdGrowth, varTot = varPlot, MapName,
                VER, sdMap, BIO, REALM, OPEN, INVENTORY, TIER)

# allPlots0 <- read.csv('validationData_PLOTS_AND_LIDAR_pvir5b.csv') #old database; could be dummy 

# allPlotsCombi <- rbind(subset(allPlots0, allPlots0$MapName == '10_GlobBiomass'),
#                        allPlotsNew) # "VER" variable for PVIR year version!!!
allPlotsCombi=allPlotsNew
p =allPlotsCombi
setwd(plotsFolder)
r=raster('/Users/renflores/Documents/GFC/data/ESACCI-BIOMASS-L4-AGB_SD-MERGED-10000m-fv5.0.tif')[[1]]
coordinates(p) <- ~POINT_X+POINT_Y
allPlotsCombi$sdMap <- ifelse(allPlotsCombi$MapName == '10_GlobBiomass', extract(r,p), NA)
r=stack('ESACCI-BIOMASS-L4-AGB_SD-MERGED-10000m-fv5.0.tif')[[2]]

write.csv(allPlotsCombi,'validationData_PLOTS_AND_LIDAR_pvir6_2010_ver20250207.csv', row.names=F)


