## FUNCTIONS TO VISUALIZE HISTOGRAM EFFECT OF TEMPORAL FIX AND ALSO SUMMARIZES IT PER AGB BIN IN A TABLE

HistoTemp <- function(df, year){
  
  if("plotAGB_10" %in% colnames(df)){
    df$AGB_T_HA <- df$plotAGB_10
    df$AGB_T_HA_ORIG <- df$orgPlotAGB
    main <- 'Before and after FF correction'
  }else {    
    df$AGB_T_HA <- df$AGB_T_HA
    df$AGB_T_HA_ORIG <- df$AGB_T_HA_ORIG
    main <- 'Before and after temporal adjustment'}

  df <- df[(df$AGB_T_HA < 600 & df$AGB_T_HA_ORIG <600 & df$AGB_T_HA > 0), ] #select 600 and below, disregard negative for now
  
  # create a bar graph with fixed agb bins
  h1 <- hist(df$AGB_T_HA_ORIG, plot=F, breaks=25)
  h2 <- hist(df$AGB_T_HA, plot=F, breaks=25)
  
  png (filename=paste0(outDir,paste0('/histogram_tempfixed_',year,'.png')),  width = 800, height = 600)
  y.ax <- nrow(df) / 4
 # y.ax <-25000
  plot(h1, xaxt="n", col=rgb(0,0,1,1/4), main=main, xlab='AGB(Mg/ha)',ylab='n',
       xlim = c(0,600), ylim=c(0,y.ax),cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2)
  axis(1,at=0:6*100, labels=c(0:6*100),cex.lab=2, cex.axis=1.5, cex.main=2, cex.sub=2)
  plot(h2,col=rgb(1,0,0,1/4),add=T)
  legend("topright", c("Before", "After", "Overlap"),
         col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(0.5,0,0.5,1/4)), lwd=10, cex=2, bty='n')
  
  dev.off()
  
  
  plot(h1, xaxt="n", col=rgb(0,0,1,1/4), main=main, xlab='AGB(Mg/ha)',ylab='n',
       xlim = c(0,600), ylim=c(0,y.ax))
  axis(1,at=0:6*100, labels=c(0:6*100))
  plot(h2,col=rgb(1,0,0,1/4),add=T)
  legend("topright", c("Before", "After", "Overlap"),
         col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(0.5,0,0.5,1/4)), lwd=10)
}

HistoShift <- function(df, year){
  if("plotAGB_10" %in% colnames(df)){
    df$AGB_T_HA <- df$plotAGB_10
    df$AGB_T_HA_ORIG <- df$orgPlotAGB
   
  }else {    
    df$AGB_T_HA <- df$AGB_T_HA
    df$AGB_T_HA_ORIG <- df$AGB_T_HA_ORIG
   }
  
  #calculate change in bins
  df$AGB_T_HA_ORIG <- ifelse(df$AGB_T_HA_ORIG == 0, df$AGB_T_HA_ORIG + 0.0001,df$AGB_T_HA_ORIG)
  df$AGB_T_HA <- ifelse(df$AGB_T_HA == 0, df$AGB_T_HA + 0.0001,df$AGB_T_HA)
  
  bins <- c(0:9*20, 2:5*100, Inf)
  old1 <- transform(df, group=cut(AGB_T_HA_ORIG,  breaks=bins))
  new1 <- transform(df, group=cut(AGB_T_HA,  breaks=bins))
  
  old2 <- old1 %>% 
    group_by(group) %>%
    tally()
  old2 <- as.data.frame(old2)
  
  new2 <- new1 %>% 
    group_by(group) %>%
    tally()
  new2 <- as.data.frame(new2)

  #calculate change in AGB
  old3 <- aggregate(old1["AGB_T_HA_ORIG"], by=old1["group"], mean)
  new3 <- aggregate(new1["AGB_T_HA"], by=new1["group"], mean)
  
  if (nrow(old2) != nrow(new2)){
    fj1 <- full_join(old2, new2, by=c('group'='group'))
    fj2 <-  full_join(old3, new3, by=c('group'='group'))
    outs <- cbind(fj1,fj2)
    outs <- outs[,c(1,2,3,5,6)]
    
  }
  else{
    outs <- do.call(cbind, list(old2,new2,old3,new3))
    outs <- outs[,c(1,2,4,6,8)]
  }
  
  names(outs) <- c('agb_Mgha_bins', 'n_pre', 'n_post', 'agb_Mgha_pre', 'agb_Mgha_post')
  setwd(outDir)
  write.csv(outs, paste0('TF_pre_post_change_',year,'.csv'), row.names = F)
  setwd(mainDir)
  print(outs)
}
