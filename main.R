###################################################
###### MAIN SCRIPT PLOT-TO-MAP COMPARISON #########
###################################################

# Authors: 
# Sytze de Bruin, Laboratory of Geo-information Science and Remote Sensing, 
# Wageningen University. e-mail: sytze.debruin@wur.nl
# Arnan Araza, Laboratory of Geo-information Science and Remote Sensing, 
# Wageningen University. e-mail: arnanaraza@wur.nl


## ------------------ Preliminaries ------------------
rm(list=ls())

# packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rgdal,rgeos,raster,plyr,dplyr,foreach,parallel,doParallel,plotrix,gfcanalysis,sf,stringr)

# global variables
mainDir <- "D:/BiomassCCI_2019"
scriptsDir <- "D:/BiomassCCI_2019/scripts" 
outDir <- "D:/BiomassCCI_2019/results"
dataDir <- "D:/BiomassCCI_2019/data"
plotsFile <- 'SamplePlots.csv'
plotsFile1 <- 'SamplePoly.csv'
agbTilesDir <- "E:/GlobBiomass2017/" #*
treeCoverDir <- 'E:/treecover2010_v3' #*
flDir <- 'E:/GFCFolder' 
forestTHs <- 10 
mapYear <- 10
SRS <- CRS('+init=epsg:4326')

    #* be sure to download/access tiles

# functions
setwd(scriptsDir)  
source('Polygonize.R')
source('Deforested.R')
source('BiomePair.R')
source('TempFix.R')
source('TempVis.R')
source('MakeBlockPolygon.R')
source('TileNames.R')
source('BlockMeans.R')
source('invDasymetry.R')
source('Plots.R')
setwd(mainDir)

## ------------------ Preliminary -------------------------------
# open plot data
loc <- list.files(dataDir, pattern='Sample')
setwd(dataDir)
plots <- read.csv(loc[1])

  # reference data is polygon?
  plotsPoly <- read.csv(loc[2])
  plotsPolyAGB <- read.csv(loc[3])
  SRS <- CRS('+init=epsg:32622')
  plots <- Polygonize(plotsPoly, SRS)

# remove deforested plots
plots1 <- Deforested(plots,flDir,mapYear) 

# get biomes and zones
plots2 <- BiomePair(plots1)
  
  #merge plot-level data for polygons
  plots2$AGB_T_HA <- plotsPolyAGB$AGB_T_HA 
  plots2$AVG_YEAR <- plotsPolyAGB$AVG_YEAR
  
## ------------------ Temporal adjustment ------------------------

# apply growth data to whole plot data by identifying AGB map year
gez <- sort(as.vector((unique(plots2$GEZ)))) #get unique gez and without NA (sorting removes it also)
plots.tf <- ldply(lapply (1:length(gez), function(x) 
  TempApply(plots2, gez[[x]], 2000)), data.frame) #change the year!

#tree growth data uncertainty estimate
plots.var <- ldply(lapply (1:length(gez), function(x) 
  TempVar(plots2, gez[[x]], 2000)), data.frame) 

#get absolute uncertainty of temporally adjusted plots 
plots.tf$sdGrowth <- abs(plots.tf$AGB_T_HA - plots.var$SD)

#order pre and post temproal fix plots for pairing
plots3 <- plots2[with(plots2, order(GEZ)), ]
plots.tf$AGB_T_HA_ORIG <- plots3$AGB_T_HA

#histogram of temporal fix effect
HistoTemp(plots.tf, 2000)
HistoShift(plots.tf, 2000)
rm(plots1, plots2, plots3, plots.var) 

  # export new AGB data according to date generated (optional)
  write.csv(plots.tf, paste0('Validation_data_TempFixed_',Sys.Date(),'.csv'), row.names=FALSE)


## ------------------ Forest fraction and plot-to-map comparison  ---------------------------

# retrieve zoning groups
continents <- unique(na.omit(plots.tf$ZONE))
biomes <- unique(na.omit(plots.tf$GEZ))

# results aggregated per 0.1 degree cell
for(continent in continents){
  cat("Processing: ",continent,"\n")
  
  AGBdata <- invDasymetry("ZONE", continent, 0.1, 5, is_poly=FALSE)
  
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
  
  AGBdata <- invDasymetry("GEZ", biome, 0.1, 5, is_poly=FALSE)
  
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
  
  AGBdata <- invDasymetry("ZONE", continent, wghts = TRUE, is_poly = F)
  
  save(AGBdata, file = file.path(outDir,
                                 paste0("InvDasyPlot_", continent, ".Rdata")))
  
  Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
         continent, paste0('binnedPlt_',continent,'_',
                           Sys.Date(),'.png'))
  
  Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
          continent, paste0('scatterPlt_',continent,'_',
                            Sys.Date(),'.png'))
}


