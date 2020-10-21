###FUNCTION TO CREATE ACCURACY TABLE AFTER PLOT-MAP VALIDATION

Accuracy <- function(df=plotsBACC, intervals=8, dir=resultsFolder, str=''){
  
  #assign AGB bins
  if (intervals == 8){
    bins <- c(-Inf,50,100,150,200,250,300,400,Inf) #7 intervals
    bins.str <-c('0-50','50-100','100-150','150-200','200-250','250-300','300-400', '>400')
  }
  if (intervals == 7){
    bins <- c(-1,50,100,150,200,250,300,Inf) #7 intervals
    bins.str <-c('0-50','050-100','100-150','150-200','200-250','250-300', '>300')
  }
  if (intervals == 6){
    bins <- c(-1,100,150,200,250,300,Inf) #6 intervals
    bins.str <-c('0-100','100-150','150-200','200-250','250-300', '>300')
  }
  
  #assign grouping of AGB values for plot and map separately per bin
  grp1 <- transform(df, group=cut(df$plotAGB_10,  breaks=bins))
  
  #aggregate the mean AGB of bins 
  agg.plot <- ddply(grp1, .(group), summarise, plotAGB_10=mean(plotAGB_10), .drop=F) 
  agg.map <- ddply(grp1, .(group), summarise, mapAGB=mean(mapAGB, na.rm=T), .drop=F)
  
  ##calculate accuracy metrics -- assures values derived are from PLOT BINS
  grp2 <- grp1[,c('plotAGB_10','mapAGB','sdPlot','sdMap','group')] #retains plotAGB, mapAGB, plotVar, group 
  
  #rmsd per mean agb bin
  msd <- grp2 %>% 
    group_by(group) %>%
    summarise(val=mean((plotAGB_10-mapAGB)^2)) #same with mse
  
  #mean((actual-predicted)^2) #mse
  #checker using msd - mean plot error (measurement)  =  map variance
  check <- grp2 %>% 
    group_by(group) %>%
    summarise(val=(mse(plotAGB_10, mapAGB) - mean(sdPlot^2)) - mean(sdMap^2, na.rm=T))
  
  #plot (measurement) variance per bin
  plotvar <- grp2 %>% 
    group_by(group) %>%
    summarise(val=mean(sdPlot^2, na.rm=T))
  
  #map SE turned variance per mean agb bin
  mapvar <- grp2 %>% 
    group_by(group) %>%
    summarise(val= mean(sdMap^2, na.rm=T))
  
  #print(sd of error)
  len <- length(bins.str) #row control
  agg.plot <- agg.plot[c(1:len),]
  agg.map <- agg.map[c(1:len),]
  
  #join accuracy metrics with original table
  df.new <- data.frame(agg.plot, agg.map)
  df.new <- left_join(df.new, msd, by = c('group'='group'))
  df.new <- left_join(df.new, plotvar, by = c('group'='group'))
  df.new <- left_join(df.new, mapvar, by = c('group'='group'))
  df.new <- left_join(df.new, check, by = c('group'='group'))
  df.new <- df.new[,-c(1,3)]
  
  #add plot tally #origin of bins, plotcount 
  plot.count <- data.frame(table(grp1$group)) #orders accordingly
  
  #combine all
  df.new <- data.frame(plot.count, df.new) 
  names(df.new) <- c('bins', 'plot_count', 'plot', 'map', 'msd','plot_var', 'map_var', 'checker')
  df.new$bins <- bins.str
  
  #add last row for totals
  col1 <- 'total'
  col2 <- sum(df.new$plot_count, na.rm=T) 
  col3 <- mean(df$plotAGB_10, na.rm=T)
  col4 <- mean(df$mapAGB, na.rm=T)
  col5 <- mse(df$plotAGB_10, df$mapAGB) 
  col6 <- mean(df$sdPlot^2, na.rm=T)
  col7 <- mean(df$sdMap^2, na.rm=T)
  col8 <- (col5+ col6) - col7 #checker
  lastrow <- data.frame(col1,col2,col3,col4,col5,col6,col7,col8)
  names(lastrow) <- names(df.new)
  df.new <- rbind(df.new,lastrow)
  
  #round df into 2 decimals 
  round_df <- function(x, digits) {
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
  }
  
  #add bias column
  df.new1 <- cbind (df.new, bias = df.new[4] - df.new[3])
  names(df.new1) <- c('AGB bin (Mg/ha)','n', 'AGBref (Mg/ha)', 'AGBmap (Mg/ha)',  
                      'RMSD','varPlot', 'varMap', 'IVar', 'AGBmap-AGBref')
  df.new1 <- na.omit(df.new1[, c('AGB bin (Mg/ha)','n', 'AGBref (Mg/ha)', 'AGBmap (Mg/ha)',
                                 'AGBmap-AGBref',
                                 'RMSD','varPlot', 'varMap', 'IVar')] )
  
  df.new1$IVar <- ifelse(df.new1$IVar < 0, 0, 1)
  df.new1$RMSD <- sqrt(df.new1$RMSD)
  df.new1 <- round_df(df.new1, 0)
  write.csv(df.new1, paste0(dir,'/acc_',str, '.csv'), row.names = F)
  return(df.new1)
}
