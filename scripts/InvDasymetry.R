invDasymetry <- function(clmn = "ZONE", value = "Europe", aggr = NULL, 
                         minPlots = 1, wghts = FALSE, is_poly=TRUE, own=TRUE,fmask=NA){
    if(class(fmask)=='RasterLayer'){
      fmask <- fmask
      plot(fmask)}
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
      
      #aggregatioN!
      plots.tf$inv <- 1/plots.tf$varPlot

      plotsTMP <- aggregate(plots.tf[,c('AGB_T_HA_ORIG', 'AGB_T_HA', 'SIZE_HA')],
                            list(plots.tf$Xnew, plots.tf$Ynew), 
                            mean, na.rm=T) 
      
      plotsTMP <- cbind(plotsTMP, aggregate(plots.tf[,"varPlot"], 
                                            list(plots.tf$Xnew, plots.tf$Ynew), function(x) 1/sum(1/x))[3])
      
      plotsTMP <- plotsTMP[with(plotsTMP, order(Group.2, Group.1)), ] #order to match
      x <- ddply(plots.tf, .(paste(plots.tf$Ynew, plots.tf$Xnew)),  
                 function(x) data.frame(Xnew=mean(x$Xnew), 
                                        Ynew=mean(x$Ynew),
                                        AGB_T_HA=weighted.mean(x$AGB_T_HA, x$inv ,na.rm=T)))
      x <- x[with(x, order(Ynew, Xnew)), ] 
      
      plotsTMP$AGB_T_HA1 <- x$AGB_T_HA
      
      names(plotsTMP) <- c("POINT_X","POINT_Y",'AGB_T_HA_ORIG','AGB_T_HA_UW',
                           'SIZE_HA', 'varPlot','AGB_T_HA')
      
      # only keep plots satisfying minPlots criterion
      if(minPlots > 1){
        blockCOUNT <- aggregate(plots.tf$AGB_T_HA, list(plots.tf$Xnew, plots.tf$Ynew), 
                                function(x) length(na.omit(x)))
        ndx <- which(blockCOUNT$x >= minPlots)
        plotsTMP1 <- plotsTMP[ndx,]
        if(nrow(plotsTMP1) < 2){plotsTMP1 <- plotsTMP[1:2,]}
        plotsTMP1$n <- subset(blockCOUNT,blockCOUNT$x >= minPlots)[[3]] #add plots inside
        print(plotsTMP1)
      }
      plots.tf <- plotsTMP1
      rsl <- aggr
    } else {
      # determine resolution output
      fname <- list.files(agbTilesDir, "*.tif")[99]
      rsl <- xres(raster(file.path(agbTilesDir, fname)))
    }
    
    #error control for few plots after aggregation
    try(if(nrow(plots.tf) <= 1) stop("very few plots selected, try decreasing minPlots or run at original resolution"))
    
    print(paste0(nrow(plots.tf), ' number of plots being processed'))
    if (own==T){rsl <- xres(AGBown)}
    # sample forest fraction and AGB data per cell/plot
    nc <- detectCores()
    cl <- makeCluster(nc-1)
    registerDoParallel(cl, nc)
    
    
    FFAGB <- foreach(i=1:nrow(plots.tf), .combine='rbind',# .errorhandling = 'pass',
                     .packages='raster', .export=c('MakeBlockPolygon', 'SRS',
                                                   'sampleTreeCover', 'TCtileNames',
                                                   'AGBtileNames', 'sampleTreeCover',
                                                   'sampleAGBmap', 'plots', #for polygon
                                                   'agbTilesDir', 'treeCoverDir', 'AGBown','fmask',
                                                   'forestTHs')) %dopar% {
                                                     
                                                     if (is_poly==TRUE){
                                                       pol <- plots[i,] #own polygon
                                                     }else{
                                                       pol <- MakeBlockPolygon(plots.tf$POINT_X[i], 
                                                                               plots.tf$POINT_Y[i], rsl)
                                                     }
                                                     if(is.null(aggr)){ #no aggregation!
                                                       if(is.na(plots.tf$SIZE_HA[i])){
                                                         treeCovers <- sampleTreeCover(pol, forestTHs, wghts, fmask)
                                                       } else if(plots.tf$SIZE_HA[i] >= 1){
                                                         # ***** if plot size equals 1 ha *****
                                                         treeCovers <- rep(1, length(forestTHs))
                                                       } else {
                                                         # ***** if plot size less than 1 ha *****
                                                         treeCovers <- sampleTreeCover(pol, forestTHs, wghts,fmask)
                                                       }
                                                     } else 
                                                       treeCovers <- sampleTreeCover(pol, forestTHs, wghts,fmask)
                                                     wghts2 <- ifelse(is.null(aggr), FALSE, wghts)
                                                     
                                                     if(!is.null(aggr)){
                                                       c(treeCovers * plots.tf$AGB_T_HA[i],
                                                         plots.tf$AGB_T_HA_UW[i], plots.tf$AGB_T_HA_ORIG[i], 
                                                         sampleAGBmap(pol, wghts2, own), 
                                                        plots.tf$SIZE_HA[i], plots.tf$n[i],
                                                        plots.tf$POINT_X[i], plots.tf$POINT_Y[i])}
                                                     else{
                                                          c(treeCovers * plots.tf$AGB_T_HA[i],plots.tf$AGB_T_HA[i],
                                                            plots.tf$AGB_T_HA_ORIG[i], 
                                                            sampleAGBmap(pol, wghts2, own), 
                                                            plots.tf$SIZE_HA[i], 
                                                            plots.tf$POINT_X[i], plots.tf$POINT_Y[i])
                                                                       }
                                                
                                                   }
    stopCluster(cl)
    FFAGB <- data.frame(FFAGB)
    FFAGB
    if (!is.null(aggr)){
      names(FFAGB) <- c(paste0("plotAGB_", forestTHs), "uwPlotAGB", "orgPlotAGB",
                                          "mapAGB",'SIZE_HA', 'n', "x", "y")}
    else{
      names(FFAGB) <- c(paste0("plotAGB_", forestTHs), "tfPlotAGB","orgPlotAGB",
                        "mapAGB",'SIZE_HA', "x", "y")
                                          }
    
    

    print(head(FFAGB))
    return(FFAGB)
  }
  


