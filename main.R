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

## Packages installation
if (!require("pacman")) install.packages("pacman")
  pacman::p_load(terra,plyr,dplyr,foreach,purrr,BIOMASS,data.table,ranger,randomForest,
               doParallel,plotrix,gfcanalysis,sf,stringr,Metrics)


## Define global variables and folder directories, adapt accordingly e.g. "C:/PlotToMap"
mainDir <- "C:/PlotToMap"
scriptsDir <- "C:/PlotToMap/scripts" 
outDir <- "C:/PlotToMap/results"
dataDir <- "C:/PlotToMap/data"
flDir <- 'C:/GFCFolder' #should be outside the main directory because GFC tiles will be downloaded here

SRS <- CRS('+init=epsg:4326')

## Set forest threshold into FAO-suggested 10% and above tree cover 
forestTHs <- 10


## Validate own map? 
AGBown <- 'NA'

## Map and tree cover directories that SHOULD BE PRE-DOWNLOADED! See download script for CCI maps. 
agbTilesDir <- "D:/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2020-fv5.0"
treeCoverDir <- 'C:/treecover2010_v3_100m'
#treeCoverDir <- 'D:/treecover2010_v3'

## The map epoch , works if 
mapYear <- as.numeric(str_sub(str_extract(agbTilesDir, "\\d{4}"), -2, -1))
#mapYear <- 20

## The map spatial resolution
mapRsl <- as.numeric(str_replace(str_extract(agbTilesDir, "\\d{3}m"), "m", ""))
#mapRsl <- 100 

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
source('invDasymetry_NoPar.R')
source('Plots.R')
source('Nested.R')
source('MeasurementErr.R')
source('RawPlots.R')
source('Accuracy.R')
source('RefLidar.R')
setwd(dataDir)


## ------------------ Preliminaries and pre-processing -------------------------------

## Five (5) cases of plot data from users. Cases 1-2 are plot-level (there is already an AGB estimate from the user),
## while Cases 3-5 are data with tree measurements where AGB and SD will be estimated using BIOMASS R package
  
  ## (1) PLOT DATA IS POINT DATA AND FORMATTED (SEE TECHNICAL DOCUMENTATION)
  
  plotsFile <- 'SamplePlots.csv'
  plots <- read.csv(plotsFile) 

  
  ## (2) PLOT DATA IS UNFORMATTED i.e. OWN FORMAT OF DATA SOURCE
  ## Then will ask users about specific column index of required plot variables such as
  ## unique plot D, longitude, latitude, AGB of the plot, plot size, inventory year
  
  plotsFile <- 'SampleUnformattedPlots.csv'
  plots <- RawPlots(read.csv(plotsFile)) 

  ## (3) PLOT DATA IS A POLYGON WITH PLOT CORNER COORDINATES
  
  plotsFile <- 'PolyTropiSAR.csv'  #Labriere et al. 2018 sample data
  plotsAGBFile <- 'PolyTropiAGB.csv'
  plotsPoly <- read.csv(plotsFile) 
  plotsPolyAGB <-  read.csv(plotsAGBFile) #plot-level AGB  
  SRS <- 32622 #plot data is projected
  plots <- Polygonize(plotsPoly, SRS)
  plots$AGB_T_HA <- plotsPolyAGB$AGB_T_HA 
  plots$AVG_YEAR <- plotsPolyAGB$AVG_YEAR

 
  ## (4) PLOT DATA HAVE TREE-LEVEL MEASUREMENT  
  ## Estimate plot-level AGB using BIOMASS R package with associated SD of measurement error
  
  ## Formatted (See Technical Documentation)
  plotTree<- read.csv(paste0(dataDir, '/SampleTree.csv')) 
  xyTree <- read.csv(paste0(dataDir,'/SampleTreeXY.csv'))
  
  ## Unformatted
  TreeRaw <- read.csv(paste0(dataDir, '/KarnatakaForest.csv')) 
  rawTree <- RawPlotsTree(TreeRaw)
  plotTree<- rawTree[[1]] 
  xyTree <- rawTree[[2]]
  
  plots <- MeasurementErr(plotTree, xyTree, 'Asia')
  
  
  ## (5) PLOT DATA WITH TREE-LEVEL MEASUREMENT AND NESTED PLOTS (SUB-PLOTS)
  
  cent <- readOGR(dsn = dataDir, layer = "SampleCentroid") #Sub-plot centroid
  tree <- read.csv(paste0(dataDir,'/SampleTreeNested.csv')) #Tree data per sub-plot
  TreeData <- Nested(cent, tree)
  plotTree <- TreeData[[1]]
  xyTree <- TreeData[[2]]
  plots <- MeasurementErr(plotTree, xyTree, 'Europe')
  
  
  ## (6) REFERENCE DATA IS A LIDAR-BASED MAP

  slb.agb.dir <- './SustainableLandscapeBrazil_v04/SLB_AGBmaps'
  slb.cv.dir <- './SustainableLandscapeBrazil_v04/SLB_CVmaps'
  slb.cv <- RefLidar(slb.cv.dir, 2018)
  plots <- RefLidar(slb.agb.dir, 2018)
  plots$sdTree <- slb.cv$CV * plots$AGB_T_HA


## Remote deforested plots until year of map epoch and assign biomes/eco-regins 
plots1 <- Deforested(plots,flDir,mapYear)
plots2 <- BiomePair(plots) 


## ------------------ Measurement error (ONLY for plot data cases #1-3) --------------------------

  ## Using a pre-trained RF model for plot-level data 
  load('rf1.RData') #pre-trained RF model from 10000+ plots across biomes 
  plotsPred <- plots2[,c('AGB_T_HA','SIZE_HA', 'GEZ')]
  names(plotsPred) <- c('agb', 'size', 'gez')
  plotsPred$size <- as.numeric(plotsPred$size) * 10000 #convert size to m2
  plotsPred$gez = factor(plotsPred$gez,levels = c("Boreal","Subtropical","Temperate","Tropical"))
  plots2$sdTree <- predict(rf1, plotsPred)[[1]]
  
  
  
## ------------------ Temporal adjustment ------------------------------------------------------
  
## Apply growth data to whole plot data by identifying AGB map year with associated SD based on
## growth data from IPCC 2019
yr <- 2000+mapYear
gez <- sort(as.vector((raster::unique(plots2$GEZ)))) 
plots3 <- ldply(lapply (1:length(gez), function(x) TempApply(plots2, gez[[x]], yr)), data.frame) 
plots.tf <- ldply(lapply (1:length(gez), function(x) TempVar(plots3, gez[[x]], yr)), data.frame) 
plots.tf$sdGrowth <- ifelse(is.nan(plots.tf$sdGrowth), mean(plots.tf$sdGrowth,na.rm=T),plots.tf$sdGrowth)

## Histogram and summary table: before and after temporal adjustment
dir.create(file.path(outDir), recursive = TRUE)
HistoTemp(plots.tf, yr)
HistoShift(plots.tf, yr)


## ------------------ Sampling error -----------------------------------------------------------
    
## Estimates SD of pixel and plot sizes differences from geo-simulations of Rejou-Mechain et al. 2014 study
  
plots.tf$RS_HA <- mapRsl^2 / 10000 
plots.tf$ratio <-  as.numeric(plots.tf$SIZE_HA) / plots.tf$RS_HA
se <- read.csv(paste0(dataDir, '/se.csv'))
rfSE <- ranger(se$cv ~ ., data=se[,c('SIZE_HA','RS_HA','ratio')])
plots.tf$sdSE <-  (predict(rfSE, plots.tf[,c('SIZE_HA', 'RS_HA', 'ratio')])[[1]] / 100) * mean(plots.tf$AGB_T_HA, na.rm=T)
  
  
  
## ------------------ Plot uncertainty total ---------------------------------------------------
  
plots.tf$varPlot <- plots.tf$sdTree^2 + plots.tf$sdSE^2 +plots.tf$sdGrowth^2 
  

  
## ------------------ Export validation (and calibration)-ready data ---------------------------
  
setwd(outDir)
write.csv(plots.tf, paste0('ValidationData_',yr,'.csv'), row.names=FALSE)
setwd(dataDir)

## ------------------ Map validation (runs forest fraction correction and plot-to-map comparisons) --------

## Retrieve regions 
continents <- unique(na.omit(plots.tf$ZONE))
biomes <- unique(na.omit(plots.tf$GEZ))


  ## Validation of GLOBAL AGB maps 

  # Non-aggregated continental results
  for(continent in continents){
    cat("Processing: ",continent,"\n")
    
    AGBdata <- invDasymetry("ZONE", continent, wghts = TRUE, is_poly =F, own=F,fmask=NA)
    
    save(AGBdata, file = file.path(outDir,paste0("InvDasyPlot_", continent, ".Rdata")))
                                   
    Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
           continent, paste0('binnedPlt_',continent,'_', Sys.Date(),'.png'))
                            
    Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
            continent, paste0('scatterPlt_',continent,'_', Sys.Date(),'.png'))
    
    Accuracy(AGBdata, 8, outDir, 'SampleRun')
  
  }
  
  # Use of own forest mask 
  setwd(dataDir)
  f <- raster('fmask_ph_10km.tif')
  
  for(continent in continents){
    cat("Processing: ",continent,"\n")
    
    AGBdata <- invDasymetry("ZONE", continent, wghts = TRUE, is_poly =F, own=F, fmask=f)
    
    save(AGBdata, file = file.path(outDir,paste0("InvDasyPlot_", continent, ".Rdata")))
    
    Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
           continent, paste0('binnedPlt_',continent,'_', Sys.Date(),'.png'))
    
    Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
            continent, paste0('scatterPlt_',continent,'_', Sys.Date(),'.png'))
    
    Accuracy(AGBdata, 8, outDir, 'SampleRun')
    
  }
  
  
  # Results aggregated per 0.1 degree cell per continent
  f <- NA
  for(continent in continents){
    cat("Processing: ",continent,"\n")
    
    AGBdata <- invDasymetry_NoPar("ZONE", continent, 0.1, 3, is_poly=FALSE, own=FALSE, fmask=f)
    
    save(AGBdata, file = file.path(outDir, paste0("agg01_", continent, ".Rdata")))
                                  
    Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
           'Wales', paste0('binned01_',continent,'_', Sys.Date(),'.png'))
                              
    Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
           continent, paste0('scatter01_',continent,'_', Sys.Date(),'.png'))
                            
    Accuracy(AGBdata, 8, outDir, 'SampleRun')
    
  }
  
  # Aggregated per 0.1 degree cell per biome
  for(biome in biomes){
    cat("Processing: ",biome,"\n")
    
    AGBdata <- invDasymetry("GEZ", biome, 0.1, 5, is_poly=FALSE,own=FALSE)
    
    save(AGBdata, file = file.path(outDir, paste0("agg01_", biome, ".Rdata")))
                                   
    Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
           biome, paste0('binned01_',biome,'_',Sys.Date(),'.png'))
                             
    Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
            biome, paste0('scatter01_',biome,'_', Sys.Date(),'.png'))
    
    TwoPlots(AGBdata[[3]], AGBdata[[4]], 
             AGBdata[[2]], AGBdata[[4]],
             biome, paste0('scatterPlt_',continent,'_',Sys.Date(),'.png'), 'harmo')
                               
  }


  ## Validation of OWN map (for non-aggregated run) 
  r.name <- 'Pedro_AGB_map_combined_2017v2.tif' 
  AGBown <- raster(paste0('D:/AGBG/data','/',r.name)) 
  
  for(continent in continents){
    cat("Processing: ",continent,"\n")
    AGBdata <- invDasymetry("ZONE", continent, wghts = TRUE, is_poly =F, own=T)
    save(AGBdata, file = file.path(outDir,  paste0("InvDasyPlot_", r.name, ".Rdata")))
    
    Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
           continent, paste0('binnedPlt_',r.name,'_',Sys.Date(),'.png'))
    
    Scatter(AGBdata$plotAGB_10,AGBdata$mapAGB,
            continent, paste0('scatterPlt_',r.name,'_', Sys.Date(),'.png'))
    
    Accuracy(AGBdata, 8, outDir, 'PedroMap')
  }
  
  for(continent in continents){
    cat("Processing: ",continent,"\n")
    AGBdata <- invDasymetry("ZONE", continent,0.1,5, is_poly =F, own=T)
    save(AGBdata, file = file.path(outDir,  paste0("agg01", r.name, ".Rdata")))
    
    Binned(AGBdata$plotAGB_10,AGBdata$mapAGB,
           continent, paste0('aggbinnedPlt_',r.name,'_',Sys.Date(),'.png'))
    Accuracy(AGBdata, 8, outDir, 'PedroMap_agg01')
  }
  
  
  ## Option to integrate your plot data to the WUR database under data-use agreement? ---------------------------
  plots.fin <- ToDatabase(plots.tf,'AFR_GHA',10)
  setwd('D:/AGBG') #should be changed to remote directory of WUR
  write.csv(plots.fin, paste0('Validation_data_2010map_AFR_GHA.csv'), row.names=FALSE)
  

## Bias is revealed i.e. map underestimation 
  
## ------------------ Uncertainty modelling -----------------------------------------------------

## The bias modelling follows a model-based approach where bias will be predicted based on 
## how related it is to environmental variables or covariates aggregated at 10 km
  

  