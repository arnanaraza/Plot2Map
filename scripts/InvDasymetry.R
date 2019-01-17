invDasymetry <- function(clmn = "ZONE", value = "Europe", aggr = NULL, 
                         minPlots = 1, wghts = FALSE){
  # returns a data.frame with (mean) AGB from plots satisfying selection 
  # criteria
  
  # temprorally-fixed plot data
  plots <- plotsNew
  
  if(is.null(aggr)) # overrule minPlots if no aggregagtion
    minPlots <- 1
  
  # select plots fulfilling selection criterion set by attr and value
  clm <- which(names(plots) == clmn)
  if(length(clm)==0) 
    stop(paste('Attribute', attr, 'not found'))
  
  ndx <- which(plots[,clm] == value)
  if(length(ndx)==0)
    stop('There are no records satisfying the selection criterion.')
  plots <- plots[ndx,]
  
  # aggregate if aggr != NULL
  if(!is.null(aggr)){
    # aggregate to aggr degree cells
    plots$Xnew <- aggr * (0.5 + plots$POINT_X %/% aggr)                   
    plots$Ynew <- aggr * (0.5 + plots$POINT_Y %/% aggr)                   
    
    plotsTMP <- aggregate(plots$AGB_T_HA, list(plots$Xnew, plots$Ynew), 
                          mean, na.rm=T)
    names(plotsTMP) <- c("POINT_X","POINT_Y","AGB_T_HA")
    
    # only keep plots satisfying minPlots criterion
    if(minPlots > 1){
      blockCOUNT <- aggregate(plots$AGB_T_HA, list(plots$Xnew, plots$Ynew), 
                              function(x) length(na.omit(x)))
      ndx <- which(blockCOUNT$x >= minPlots)
      plotsTMP <- plotsTMP[ndx,]
    }
    plots <- plotsTMP
    rsl <- aggr
  } else {
    # determine resolution output
    fname <- list.files(agbTilesDir, "*.tif")[1]
    rsl <- xres(raster(file.path(agbTilesDir, fname)))
  }
  
  #error control for few plots after aggregation
  try(if(nrow(plots) <= 1) stop("too few plots selected, try decreasing minPlots or run non-aggregated model"))
  
  print(paste0(nrow(plots), ' number of plots being processed'))
  
  # sample forest fraction and AGB data per cell/plot
  nc <- detectCores()
  cl <- makeCluster(nc-1)
  registerDoParallel(cl, nc)
  
  
  FFAGB <- foreach(i=1:nrow(plots), .combine='rbind', .errorhandling = 'remove',
                   .packages='raster', .export=c('MakeBlockPolygon', 'SRS',
                                                 'sampleTreeCover', 'TCtileNames',
                                                 'AGBtileNames', 'sampleTreeCover',
                                                 'sampleAGBmap',
                                                 'agbTilesDir', 'treeCoverDir',
                                                 'forestTHs')) %dopar% {
                                                   pol <- MakeBlockPolygon(plots$POINT_X[i], 
                                                                           plots$POINT_Y[i], rsl)
                                                   if(is.null(aggr)){
                                                     if(is.na(plots$SIZE_HA[i])){
                                                       treeCovers <- sampleTreeCover(pol, forestTHs, wghts)
                                                     } else if(plots$SIZE_HA[i] >= 1){
                                                       # ***** if plot size equals 1 ha *****
                                                       treeCovers <- rep(1, length(forestTHs))
                                                     } else {
                                                       # ***** if plot size less than 1 ha *****
                                                       treeCovers <- sampleTreeCover(pol, forestTHs, wghts)
                                                     }
                                                   } else
                                                     treeCovers <- sampleTreeCover(pol, forestTHs, wghts)
                                                   wghts2 <- ifelse(is.null(aggr), FALSE, wghts)
                                                   c(treeCovers * plots$AGB_T_HA[i], plots$AGB_T_HA[i], 
                                                     sampleAGBmap(pol, wghts2), plots$POINT_X[i], 
                                                     plots$POINT_Y[i])
                                                 }
  stopCluster(cl)
  FFAGB <- data.frame(FFAGB)
  names(FFAGB) <- c(paste0("plotAGB_", forestTHs), "orgPlotAGB", 
                    "mapAGB", "x", "y")
  return(FFAGB)
}
