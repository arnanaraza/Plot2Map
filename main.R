###################################################
###### MAIN SCRIPT GLOBBIOMASS VALIDATION #########
###################################################

# Author 1: Sytze de Bruin, Laboratory of Geo-information Science and Remote Sensing, 
# Wageningen University. e-mail: sytze.debruin@wur.nl
# Author 2: Arnan Araza, Laboratory of Geo-information Science and Remote Sensing, 
# Wageningen University. e-mail: arnanaraza@wur.nl


## ------------------ Preliminaries ------------------
rm(list=ls())

# packages
library(rgdal)
library(raster)
library(plyr)
library(dplyr)
library(foreach)
library(parallel)
library(doParallel)
library(plotrix)


# global variables
mainDir <- "M:/BiomassCCI_2019"
scriptsDir <- "M:/BiomassCCI_2019/scripts" 
outDir <- "M:/BiomassCCI_2019/results"
dataDir <- "M:/BiomassCCI_2019/data"
plotsFile <- 'SamplePlots.csv'
agbTilesDir <- "D:/GlobBiomass_global_biomass_product_v20180531/agb" #*
treeCoverDir <- '//GRS_NAS_01/GRSData/global_products/Hansen/treecover_2010/treecover2010_v3' #*
SRS <- CRS("+init=epsg:4326")
forestTHs <- 10 

    #* dataset should be in tiles


# functions
setwd(scriptsDir)  
source('TempFixed.R')
source('TempEffect.R')
source('MakeBlockPolygon.R')
source('TileNames.R')
source('BlockMeans.R')
source('invDasymetry.R')
source('Plots.R')
setwd(mainDir)

## ------------------ Temporal adjustment ------------------------

# open plot data
loc <- list.files(dataDir, pattern=plotsFile) 
setwd(dataDir)
plots <- read.csv(loc[1])

# apply growth data to whole plot data by identifying AGB map year
gez <- sort(as.vector((unique(plots$GEZ)))) #gets unique eco-zones without NAs
plotsNew <- ldply(lapply (1:length(gez), function(x) 
  TempFixed(plots, gez[[x]], 2010)), data.frame) #2010 = GlobBiomass year

# add plots with NAs from the "uniques" 
plotsNew <- rbind(plotsNew, subset(plots, is.na(GEZ)))

# creates histogram and change table of pre and post temporal fix
hist <- HistoShift(plots, plotsNew)
change <- ChangeTable(plots, plotsNew)
rm(plots)

  # export new AGB data according to date generated (optional)
  write.csv(plotsNew, paste0('GlobBiomass_validation_data_600_TempFixed_',Sys.Date(),'.csv'), row.names=FALSE)


## ------------------ InvDasymetry model ---------------------------

# retrieve zoning groups
continents <- unique(na.omit(plotsNew$ZONE))
biomes <- unique(na.omit(plotsNew$GEZ))

# results aggregated per 0.1 degree cell
for(continent in continents){
  cat("Processing: ",continent,"\n")
  
  AGBdata <- invDasymetry("ZONE", continent, 0.1, 5)
  
  save(AGBdata, file = file.path(outDir,
                                 paste0("agg01_", continent, ".Rdata")))
  
  Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
         continent, paste0('binned01_',continent,'_',
                               Sys.Date(),'.png'))
  
  Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
         continent, paste0('scatter01_',continent,'_',
                           Sys.Date(),'.png'))
}

# aggregated per 0.1 degree cell per biome
for(biome in biomes){
  cat("Processing: ",biome,"\n")
  
  AGBdata <- invDasymetry("GEZ", biome, 0.1, 5)
  
  save(AGBdata, file = file.path(outDir, 
                                 paste0("agg01_", biome, ".Rdata")))
  
  Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
         biome, paste0('binned01_',biome,'_',
                           Sys.Date(),'.png'))
  
  Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
          biome, paste0('scatter01_',biome,'_',
                            Sys.Date(),'.png'))
}

# non-aggregated results
for(continent in continents){
  cat("Processing: ",continent,"\n")
  
  AGBdata <- invDasymetry("ZONE", continent, wghts = TRUE)
  
  save(AGBdata, file = file.path(outDir,
                                 paste0("InvDasyPlot_", continent, ".Rdata")))
  
  Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
         continent, paste0('binnedPlt_',continent,'_',
                           Sys.Date(),'.png'))
  
  Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
          continent, paste0('scatterPlt_',continent,'_',
                            Sys.Date(),'.png'))
}


