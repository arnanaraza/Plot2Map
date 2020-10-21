###################################################
###### MAIN SCRIPT PLOT-TO-MAP COMPARISON #########
###################################################

# Authors: 
# Arnan Araza, Laboratory of Geo-information Science and Remote Sensing, 
# Wageningen University. e-mail: arnanaraza@wur.nl
# Sytze de Bruin, Laboratory of Geo-information Science and Remote Sensing, 
# Wageningen University. e-mail: sytze.debruin@wur.nl


## ------------------ Preliminaries ------------------
rm(list=ls())

# packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rgdal,rgeos,raster,plyr,dplyr,foreach,purrr,BIOMASS,data.table,
               parallel,doParallel,plotrix,gfcanalysis,sf,stringr, randomForest,BIOMASS)

# global variables
mainDir <- "D:/BiomassCCI_2019"
scriptsDir <- "D:/BiomassCCI_2019/scripts" 
outDir <- "D:/BiomassCCI_2019/results"
dataDir <- "D:/BiomassCCI_2019/data"
#plotsFile <- 'SamplePlots.csv'
#plotsFile1 <- 'SamplePoly.csv'
#plotsFile1 <- 'PolyTropiSAR.csv'
agbTilesDir <- "E:/CCIBiomass" #*
treeCoverDir <- 'E:/treecover2010_v3' #*
SRS <- CRS('+init=epsg:4326')
flDir <- 'E:/GFCFolder' 
forestTHs <- 10 
mapYear <- 19
AGBown <- 'NA'
plots <- 'NA'
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
source('Nested.R')
source('MeasurementErr.R')
setwd(mainDir)

## ------------------ Preliminary -------------------------------

# reference data is point data?
loc <- list.files(dataDir, pattern='Tropi') #tropiSAR data
#loc <- list.files(dataDir, pattern='Sample') #sample data
setwd(dataDir)
plots <- read.csv(loc[1]) #sample global data

  # reference data is polygon?
  plotsPoly <- read.csv(loc[2])
  plotsPolyAGB <- read.csv(loc[3])
  SRS <- CRS('+init=epsg:32622')  #tropiSAR data
  plots <- Polygonize(plotsPoly, SRS)
  SRS <- CRS('+init=epsg:4326') #set global SRS again
  
## ------------------ Pre-processing ----------------------------
  
# remove deforested plots  
plots1 <- Deforested(plots,flDir,mapYear) 
  
# get biomes and zones
plots2 <- BiomePair(plots1)

  #merge plot-level data for polygons
  plots2$AGB_T_HA <- plotsPolyAGB$AGB_T_HA 
  plots2$AVG_YEAR <- plotsPolyAGB$AVG_YEAR
    
  
  # reference data has tree-level measurement? -- needs centroid shp and tree data table
  cent <- readOGR(dsn = dataDir, layer = "SampleCentroid") #Wales sample data
  tree <- read.csv(paste0(dataDir,'/SampleTreeNested.csv'))
  TreeData <- Nested(cent, tree)
  plotTree <- TreeData[[1]]
  xyTree <- TreeData[[2]]
  plotTree<- read.csv(paste0(dataDir, '/SampleTree.csv')) 
  xyTree <- read.csv(paste0(dataDir,'/SampleTreeXY.csv'))
  plotTree$id <- factor(plotTree$id, levels=unique(plotTree$id), labels=seq_along(nrow(plotTree)))
  xyTree$id <- factor(xyTree$id, levels=unique(xyTree$id), labels=seq_along(nrow(xyTree)))


## ------------------ Measurement error --------------------------
  
  #option1 - using BIOMASS package, needs two inputs (tree-level data and plot coordinates)
  plots <- MeasurementErr(plotTree, xyTree, 'Europe')#world
  plots1 <- Deforested(plots,flDir,mapYear) 
  plots2 <- BiomePair(plots1)
  
  #option2 - using a pre-trained RF model for plot-level data (points and polygons)
  rf <- get(load(paste0(dataDir, '/rf1.RData'))[1]) #pre-trained RF model from 8000+ plots across biomes 
  plotsPred <- plots2[,c('AGB_T_HA','SIZE_HA', 'GEZ')]
  names(plotsPred) <- c('agb', 'size', 'gez')
  plotsPred$size <- plotsPred$size * 10000 #convert size to m2
  plotsPred$size <- as.integer(plotsPred$size)
  plotsPred$gez = factor(plotsPred$gez,
                         levels = c("Boreal","Subtropical","Temperate","Tropical"))
  plots2$sdTree <- predict(rf, plotsPred)
  
  
## ------------------ Temporal adjustment ------------------------

# apply growth data to whole plot data by identifying AGB map year
gez <- sort(as.vector((unique(plots2$GEZ)))) #get unique gez and without NA (sorting removes it also)
plots.tf <- ldply(lapply (1:length(gez), function(x) 
  TempApply(plots2, gez[[x]], 2010)), data.frame) #change the year!

#tree growth data uncertainty estimate
plots.var <- ldply(lapply (1:length(gez), function(x) 
  TempVar(plots2, gez[[x]], 2010)), data.frame) 

#get absolute uncertainty of temporally adjusted plots 
plots.tf$sdGrowth <- abs(plots.tf$AGB_T_HA - plots.var$SD)

#order pre and post temproal fix plots for pairing
plots3 <- plots2[with(plots2, order(GEZ)), ]
plots.tf$AGB_T_HA_ORIG <- plots3$AGB_T_HA

#histogram of temporal fix effect
HistoTemp(plots.tf, 2017)
HistoShift(plots.tf, 2017)
rm(plots1, plots2, plots3, plots.var) 

  # export new AGB data according to date generated (optional)
  write.csv(plots.tf, paste0('Validation_data_TempFixed_',Sys.Date(),'.csv'), row.names=FALSE)

  # if you will skip temporal adjustment
  plots2$AGB_T_HA_ORIG <- plots2$AGB_T_HA
  plots2$sdGrowth <- 0
  plots.tf <- plots2
  
  # use own map! only works for non-aggregated run
  AGBown <- 'tropiSAR_100m.tif'
  AGBown <- raster('D:/GEOCARBON/geocarbon.tif')
  AGBown <- raster(paste0(dataDir,'/',AGBown))
  AGBown [AGBown==0] <- NA


#sum uncertainty from tree measurement and temporal adjustment
plots.tf$varPlot <- plots.tf$sdTree^2 + plots.tf$sdGrowth^2
  
  
  
## ------------------ Forest fraction and plot-to-map comparison of global biomass maps ---------------------------

# retrieve zoning groups
continents <- unique(na.omit(plots.tf$ZONE))
biomes <- unique(na.omit(plots.tf$GEZ))

# non-aggregated results
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
  
  AGBdata <- invDasymetry("ZONE", continent, 0.1, 10, is_poly=FALSE, own=FALSE)
  
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



