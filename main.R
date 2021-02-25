###################################################
###### MAIN SCRIPT PLOT-TO-MAP COMPARISON #########
###################################################

# Authors: 
# Arnan Araza, Laboratory of Geo-information Science and Remote Sensing, 
# Wageningen University. e-mail: arnanaraza@wur.nl
# Sytze de Bruin, Laboratory of Geo-information Science and Remote Sensing, 
# Wageningen University. e-mail: sytze.debruin@wur.nl

# INDETED CODES ARE OPTIONAL 

## ------------------ Preliminaries ------------------
rm(list=ls())

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rgdal,rgeos,raster,plyr,dplyr,foreach,purrr,BIOMASS,data.table,ranger,
               parallel,doParallel,plotrix,gfcanalysis,sf,stringr, randomForest,BIOMASS)

# Global variables, adapt accordingly e.g. "C:/PlotToMap"
mainDir <- "C:/PlotToMap"
scriptsDir <- "C:/PlotToMap/scripts" 
outDir <- "C:/PlotToMap/results"
dataDir <- "C:/PlotToMap/data"
plotsFile <- 'SamplePlots.csv'
SRS <- CRS('+init=epsg:4326')
flDir <- 'C:/GFCFolder' 

forestTHs <- 10 
mapYear <- 18
mapRsl <- 100
AGBown <- 'NA'
plots <- 'NA'

agbTilesDir <- "C:/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2010-fv1.0" #*
treeCoverDir <- 'C:/treecover2010_v3_100m' #*
   
  #* make sure to download/access CCI maps and tree cover tiles

# Load all the functions needed
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
source('Nested.R')
source('MeasurementErr.R')
source('RawPlots.R')
setwd(dataDir)

## ------------------ Preliminaries and preprocessing -------------------------------

# 1. PLOT DATA IS POINT DATA AND FORMATTED (SEE TECHNICAL DOCUMENTATION)?
plots <- read.csv(plotsFile) 
# remove deforested plots until the year of the map "map year"
plots1 <- Deforested(plots,flDir,mapYear) 
# get biomes and zones
plots2 <- BiomePair(plots)

  # 2. PLOT DATA IS UNFORMATTED i.e. USING DEFAULT FORMAT OF THE SURVEY?
  #asks users about specific column index of required plot variables (id, x, y, agb, size, year)
  plotsFile <- 'SampleUnformattedPlots.csv'
  plots <- RawPlots(read.csv(plotsFile)) # 3 8 5 4 11 10 index
  plots1 <- Deforested(plots[1:100,],flDir,mapYear) 
  plots2 <- BiomePair(plots1)
  
  # 3. PLOT DATA IS A POLYGON WITH PLOT CORNER COORDINATES? 
  plotsFile <- 'PolyTropiSAR.csv'  #Labriere et al. 2018 sample data
  plotsPoly <- read.csv(plotsFile)
  plotsAGBFile <- 'PolyTropiAGB.csv'
  plotsPolyAGB <-  read.csv(plotsAGBFile) #agb per plot 
  SRS <- CRS('+init=epsg:32622') #plot data is projected
  plots <- Polygonize(plotsPoly, SRS)
  SRS <- CRS('+init=epsg:4326') 
  plots1 <- Deforested(plots,flDir,mapYear) 
  plots2 <- BiomePair(plots1)
  plots2$AGB_T_HA <- plotsPolyAGB$AGB_T_HA 
  plots2$AVG_YEAR <- plotsPolyAGB$AVG_YEAR

  # 4. PLOT DATA HAS TREE-LEVEL MEASUREMENT? 
  plotTree<- read.csv(paste0(dataDir, '/SampleTree.csv')) 
  xyTree <- read.csv(paste0(dataDir,'/SampleTreeXY.csv'))
  plotTree$id <- factor(plotTree$id, levels=unique(plotTree$id), labels=seq_along(nrow(plotTree)))
  xyTree$id <- factor(xyTree$id, levels=unique(xyTree$id), labels=seq_along(nrow(xyTree)))
  #estimate plot-level AGB using BIOMASS package 
  plots <- MeasurementErr(plotTree, xyTree, 'World')#includes an SD of measurement error
  plots1 <- Deforested(plots,flDir,mapYear) 
  plots2 <- BiomePair(plots)
  
  # 5. SPECIAL CASE PLOT DATA WITH TREE-LEVEL MEASUREMENT FROM A DATABASE
  cent <- readOGR(dsn = dataDir, layer = "SampleCentroid") #Plot centroid
  tree <- read.csv(paste0(dataDir,'/SampleTreeNested.csv')) #Tree data table
  TreeData <- Nested(cent, tree) #10 24 20 Picea sitchensis 100
  plotTree <- TreeData[[1]]
  xyTree <- TreeData[[2]]
  plots <- MeasurementErr(plotTree, xyTree, 'World')
  plots1 <- Deforested(plots,flDir,mapYear) 
  plots2 <- BiomePair(plots1)
  

## ------------------ Measurement error (ONLY for cases #1-3) --------------------------

  #Using a pre-trained RF model for plot-level data 
  get(load(paste0(dataDir, '/rf1.RData'))) #pre-trained RF model from 10000+ plots across biomes 
  plotsPred <- plots2[,c('AGB_T_HA','SIZE_HA', 'GEZ')]
  names(plotsPred) <- c('agb', 'size', 'gez')
  plotsPred$size <- plotsPred$size * 10000 #convert size to m2
  plotsPred$gez = factor(plotsPred$gez,
                         levels = c("Boreal","Subtropical","Temperate","Tropical"))
  plots2$sdTree <- predict(rf1, plotsPred)[[1]]
  
  
## ------------------ Temporal adjustment ------------------------
# apply growth data to whole plot data by identifying AGB map year
yr <- 2000+mapYear
gez <- sort(as.vector((unique(plots2$GEZ)))) #get unique gez and without NA (sorting removes it also)
plots3 <- ldply(lapply (1:length(gez), function(x) 
  TempApply(plots2, gez[[x]], yr)), data.frame) #make sure mapYear is set!

#tree growth data uncertainty estimate
plots.tf <- ldply(lapply (1:length(gez), function(x) 
  TempVar(plots3, gez[[x]], yr)), data.frame) 

#histogram of temporal fix effect
dir.create(file.path(outDir), recursive = TRUE)
HistoTemp(plots.tf, yr)
HistoShift(plots.tf, yr)

  # if you will skip temporal adjustment
  plots2$AGB_T_HA_ORIG <- plots2$AGB_T_HA
  plots2$sdGrowth <- 0
  plots.tf <- plots2
  
## ------------------ Sampling error ---------------------
plots.tf$RS_HA <- mapRsl^2 / 10000 
plots.tf$ratio <-  plots.tf$SIZE_HA / plots.tf$RS_HA
se <- read.csv(paste0(dataDir, '/se.csv'))
rfSE <- ranger(se$cv ~ ., data=se[,c('SIZE_HA','RS_HA','ratio')])
plots.tf$sdSE <-  (predict(rfSE, plots.tf[,c('SIZE_HA', 'RS_HA', 'ratio')])[[1]] / 100) * mean(plots.tf$AGB_T_HA, na.rm=T)

## ------------------ Plot uncertainty total ------------
plots.tf$varPlot <- plots.tf$sdTree^2 + plots.tf$sdGrowth^2 +plots.tf$sdSE^2
  
#export new validation data
setwd(outDir)
write.csv(plots.tf, paste0('Validation_data_2017map_',Sys.Date(),'.csv'), row.names=FALSE)
setwd(dataDir)

## ------------------ In-house map validation of CCI maps / plot-to-map comparison of global biomass maps ---------------------------

  
# retrieve regions 
continents <- unique(na.omit(plots.tf$ZONE))
biomes <- unique(na.omit(plots.tf$GEZ))

# non-aggregated continental results
for(continent in continents){
  cat("Processing: ",continent,"\n")
  
  AGBdata <- invDasymetry("ZONE", continent, wghts = TRUE, is_poly =F, own=F)
  
  save(AGBdata, file = file.path(outDir,
                                 paste0("InvDasyPlot_", continent, ".Rdata")))
  
  Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
         continent, paste0('binnedPlt_',continent,'_',
                           Sys.Date(),'.png'))
  
  Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
          continent, paste0('scatterPlt_',continent,'_',
                            Sys.Date(),'.png'))

}

# results aggregated per 0.1 degree cell
for(continent in continents){
  cat("Processing: ",continent,"\n")
  
  AGBdata <- invDasymetry("ZONE", continent, 0.1, 5, is_poly=FALSE, own=FALSE)
  
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
  
  AGBdata <- invDasymetry("GEZ", biome, 0.1, 5, is_poly=FALSE,own=FALSE)
  
  save(AGBdata, file = file.path(outDir, 
                                 paste0("agg01_", biome, ".Rdata")))
  
  Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
         biome, paste0('binned01_',biome,'_',
                           Sys.Date(),'.png'))
  
  Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
          biome, paste0('scatter01_',biome,'_',
                            Sys.Date(),'.png'))
  
  TwoPlots(AGBdata[[3]], AGBdata[[4]], 
           AGBdata[[2]], AGBdata[[4]],
           biome, paste0('scatterPlt_',continent,'_',
                             Sys.Date(),'.png'), 'harmo')
}

## ------------------ In-house map validation of own map ---------------------------

  # validating own maps, only works for non-aggregated run
  AGBown <- 'tropiSAR_100m.tif' #use own map!
  AGBown <- raster(paste0(dataDir,'/',AGBown)) 
  AGBown [AGBown==0] <- NA
  
  