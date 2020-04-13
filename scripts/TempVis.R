## FUNCTIONS TO VISUALIZE HISTOGRAM EFFECT OF TEMPORAL FIX AND ALSO SUMMARIZES IT PER AGB BIN IN A TABLE

HistoTemp <- function(df, year){
  
  if("plotAGB_10" %in% colnames(df)){
    df$AGB_T_HA <- df$plotAGB_10
    main <- 'Before and after plot harmonization'
  }else {    df$AGB_T_HA <- df$AGB_T_HA
  main <- 'Before and after temporal adjustment'}

  df <- df[(df$AGB_T_HA < 600 & df$AGB_T_HA_ORIG <600 & df$AGB_T_HA > 0), ] #select 600 and below, disregard negative for now
  
  # create a bar graph with fixed agb bins
  h1 <- hist(df$AGB_T_HA_ORIG, plot=F, breaks=25)
  h2 <- hist(df$AGB_T_HA, plot=F, breaks=25)
  png (filename=paste0(outDir,paste0('/histogram_tempfixed_',year,'.png')))
  y.ax <- nrow(df) / 4
 # y.ax <-25000
  
  plot(h1, xaxt="n", col=rgb(0,0,1,1/4), main=main, xlab='AGB_T_HA',ylab='Frequency',
       xlim = c(0,600), ylim=c(0,y.ax))
  axis(1,at=0:6*100, labels=c(0:6*100))
  plot(h2,col=rgb(1,0,0,1/4),add=T)
  legend("topright", c("Before", "After", "Overlap"), 
         col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(0.5,0,0.5,1/4)), lwd=10)
  
  dev.off()
  
  
  plot(h1, xaxt="n", col=rgb(0,0,1,1/4), main=main, xlab='AGB_T_HA',ylab='Frequency',
       xlim = c(0,600), ylim=c(0,y.ax))
  axis(1,at=0:6*100, labels=c(0:6*100))
  plot(h2,col=rgb(1,0,0,1/4),add=T)
  legend("topright", c("Before", "After", "Overlap"), 
         col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(0.5,0,0.5,1/4)), lwd=10)
}

HistoShift <- function(df, year){
  
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
  agg.old <- aggregate(old1["AGB_T_HA_ORIG"], by=old1["group"], mean)
  agg.new <- aggregate(new1["AGB_T_HA"], by=new1["group"], mean)
  
  outs <- do.call(cbind, list(old2,new2,agg.old,agg.new))
  outs <- outs[,c(1,2,4,6,8)]
  names(outs) <- c('agb_Mgha_bins', 'n_pre', 'n_post', 'agb_Mgha_pre', 'agb_Mgha_post')
  setwd(outDir)
  write.csv(outs, paste0('TF_pre_post_change_',year,'.csv'), row.names = F)
  setwd(mainDir)
  print(outs)
}
