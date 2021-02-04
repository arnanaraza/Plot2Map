### SCRIPT TO CALCULATE TREE TO PLOT BIOMASS UNCERTAINTY USING THE R-BIOMASS PACKAGE

## Function to calculate plot-level uncertainties taking tree-level data (plot), plot locations (xy)
MeasurementErr <- function(plot=plotIND, xy=xyIND, region='India'){
  plot <- subset(plot, diameter>=10) #filter those above 10cm in diameter
#  blowup <- plot[1,5] / 10000
 # print(paste('plot size is', blowup, 'ha'))

  #taxonomy correction
  tax <- correctTaxo(genus = plot$genus, species = plot$species)
  plot$genus <- tax$genusCorrected
  plot$species <- tax$speciesCorrected
  
  #get wood density
  wd <- getWoodDensity(genus = plot$genus,
                        species = plot$species,
                        stand = plot$id, region=region)
  plot$wd <- wd$meanWD
  plot$sd.wd<- wd$sdWD

  #compute local HD model / input your own H data if you have
  if("height" %in% colnames(plot)){
    print('using actual height data...')
  }else{
    print('using HDmodel...')
    HDmodel <- modelHD(D = NouraguesHD$D, H = NouraguesHD$H, 
                       method='weibull',  useWeight = TRUE)
    dataHlocal <- retrieveH(D = plot$diameter, model = HDmodel)
    plot$height <- dataHlocal$H
  }

  #run MC simulation
  if("height" %in% colnames(plot)){
    mc <- by(plot, plot$id,
             function(x) AGBmonteCarlo(D = x$diameter, WD = x$wd, errWD = x$sd.wd,
                                       H = x$height, errH = x$height*0.5, Dpropag ='chave2004'),simplify = F)  #assumes 30% height error
  }else{
    mc <- by(plot, plot$id,
             function(x) AGBmonteCarlo(D = x$diameter, WD = x$wd, errWD = x$sd.wd,
                                       HDmodel = HDmodel, Dpropag = "chave2004"),simplify = F)}
  
  #get agb and sd
  agb <- unlist(sapply(mc, "[", 1))
  sd <- unlist(sapply(mc, "[", 3))
  

  #add XY
  plot.fin <- left_join(plot, xy, by = c('id' = 'id')) #needs full to avoid gaps

  #remove unecessaries
  plot.fin <- plot.fin[,c("id","x","y", 'size', 'year')] # retain columns of interest
  
  #summarize per plot and add key results
  plot.fin$x <- as.numeric(plot.fin$x)
  plot.fin$y <- as.numeric(plot.fin$y)
  
  plot.fin <- plot.fin %>% group_by(id) %>% summarise_all(funs(mean)) 
  
  #scale values per ha
  agb <- agb / (plot.fin$size/10000)
  sd <- sd / (plot.fin$size/10000)
  plot.fin$agb <- agb
  plot.fin$sd <- sd
  plot.fin <- as.data.frame(plot.fin[,c("id","x","y", 'size', 'year', 'agb', 'sd')]) # retain columns of interest
  plot.fin$size <- plot.fin$size / 10000
  names(plot.fin) <- c('pltID', 'POINT_X', 'POINT_Y', 'SIZE_HA', 'AVG_YEAR', 
                       'AGB_T_HA', 'sdTree')
  return(plot.fin)
 
}
