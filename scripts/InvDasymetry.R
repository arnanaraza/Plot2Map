invDasymetry <- function(clmn = "ZONE", value = "Europe", aggr = NULL, 
                         minPlots = 1, wghts = FALSE, is_poly=TRUE){
    # returns a data.frame with (mean) AGB from plots satisfying selection 
    # criteria
    
    if(is.null(aggr)) # overrule minPlots if no aggregagtion
      minPlots <- 1
    
    # select plots fulfilling selection criterion set by attr and value
    clm <- which(names(plots.tf) == clmn)
    if(length(clm)==0) 
      stop(paste('Attribute', attr, 'not found'))
    
    ndx <- which(plots.tf[,clm] == value)
    if(length(ndx)==0)
      stop('There are no records satisfying the selection criterion.')
    plots.tf <- plots.tf[ndx,]
    
    # aggregate if aggr != NULL
    if(!is.null(aggr)){
      # aggregate to aggr degree cells
      plots.tf$Xnew <- aggr * (0.5 + plots.tf$POINT_X %/% aggr)                   
      plots.tf$Ynew <- aggr * (0.5 + plots.tf$POINT_Y %/% aggr)                   
      
      plotsTMP <- aggregate(plots.tf$AGB_T_HA,plots.tf$AGB_T_HA_ORIG, list(plots.tf$Xnew, plots.tf$Ynew), 
                            mean, na.rm=T)
      names(plotsTMP) <- c("POINT_X","POINT_Y","AGB_T_HA", 'AGB_T_HA_ORIG')
      
      # only keep plots satisfying minPlots criterion
      if(minPlots > 1){
        blockCOUNT <- aggregate(plots.tf$AGB_T_HA, list(plots.tf$Xnew, plots.tf$Ynew), 
                                function(x) length(na.omit(x)))
        ndx <- which(blockCOUNT$x >= minPlots)
        plotsTMP <- plotsTMP[ndx,]
      }
      plots.tf <- plotsTMP
      rsl <- aggr
    } else {
      # determine resolution output
      fname <- list.files(agbTilesDir, "*.tif")[1]
      rsl <- xres(raster(file.path(agbTilesDir, fname)))
    }
    
    #error control for few plots after aggregation
    try(if(nrow(plots.tf) <= 1) stop("very few plots selected, try decreasing minPlots or run at original resolution"))
    
    print(paste0(nrow(plots.tf), ' number of plots being processed'))
    
    # sample forest fraction and AGB data per cell/plot
    nc <- detectCores()
    cl <- makeCluster(nc-1)
    registerDoParallel(cl, nc)
    
    
    FFAGB <- foreach(i=1:nrow(plots.tf), .combine='rbind', .errorhandling = 'pass',
                     .packages='raster', .export=c('MakeBlockPolygon', 'SRS',
                                                   'sampleTreeCover', 'TCtileNames',
                                                   'AGBtileNames', 'sampleTreeCover',
                                                   'sampleAGBmap',  'plots',
                                                   'agbTilesDir', 'treeCoverDir',
                                                   'forestTHs')) %dopar% {
                                                     
                                                     if (is_poly==T){
                                                       pol <- plots[i,]
                                                     }else{
                                                       pol <- MakeBlockPolygon(plots.tf$POINT_X[i], 
                                                                               plots.tf$POINT_Y[i], rsl)
                                                     }
                                                     if(is.null(aggr)){
                                                       if(is.na(plots.tf$SIZE_HA[i])){
                                                         treeCovers <- sampleTreeCover(pol, forestTHs, wghts)
                                                       } else if(plots.tf$SIZE_HA[i] >= 1){
                                                         # ***** if plot size equals 1 ha *****
                                                         treeCovers <- rep(1, length(forestTHs))
                                                       } else {
                                                         # ***** if plot size less than 1 ha *****
                                                         treeCovers <- sampleTreeCover(pol, forestTHs, wghts)
                                                       }
                                                     } else
                                                       treeCovers <- sampleTreeCover(pol, forestTHs, wghts)
                                                     wghts2 <- ifelse(is.null(aggr), FALSE, wghts)
                                                     c(treeCovers * plots.tf$AGB_T_HA[i], plots.tf$AGB_T_HA_ORIG[i], 
                                                       sampleAGBmap(pol, wghts2), plots.tf$POINT_X[i], 
                                                       plots.tf$POINT_Y[i])
                                                   }
    stopCluster(cl)
    FFAGB <- data.frame(FFAGB)
    FFAGB
    names(FFAGB) <- c(paste0("plotAGB_", forestTHs), "orgPlotAGB", 
                      "mapAGB", "x", "y")
    return(FFAGB)
  }
  


