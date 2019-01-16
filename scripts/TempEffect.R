## FUNCTIONS TO VISUALIZE HISTOGRAM EFFECT OF TEMPORAL FIX AND ALSO SUMMARIZES IT PER AGB BIN IN A TABLE

HistoShift <- function(old=plots, new=plotsNew){
  old <- old[with(old, order(old$POINT_X, old$POINT_Y)), ]
  new <- new[with(new, order(new$POINT_X, new$POINT_Y)), ]
  
  
  # bind the agb columns and get other necessary columns for plotting purposes
  plots.2 <- cbind(old, new)
  plots.3 <- plots.2[,c(1,5,6,7,20,26)]
  plots.4 <- plots.3[(plots.3$AGB_T_HA < 600 & plots.3$AGB_T_HA.1 <600 & plots.3$AGB_T_HA.1 > 0), ] #0-600 Mg/ha window
  
  # create a bar graph with fixed agb bins
  h1 <- hist(plots.4$AGB_T_HA, plot=F, breaks=25)
  h2 <- hist(plots.4$AGB_T_HA.1, plot=F, breaks=25)
  
  
  png (filename=paste0(outDir, paste0('/histogram_tempfix_',Sys.Date(),'.png')))
  
  plot(h1, xaxt="n", col=rgb(0,0,1,1/4), main='Before and after temporal fix', xlab='AGB_T_HA',ylab='Frequency',
       xlim = c(0,600))
  axis(1,at=0:6*100, labels=c(0:6*100))
  plot(h2,col=rgb(1,0,0,1/4),add=T)
  legend("topright", c("Before", "After", "Overlap"), 
         col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(0.5,0,0.5,1/4)), lwd=10)
  
  dev.off()
  
  
  plot(h1, xaxt="n", col=rgb(0,0,1,1/4), main='Before and after temporal fix', xlab='AGB_T_HA',ylab='Frequency',
       xlim = c(0,600))
  axis(1,at=0:6*100, labels=c(0:6*100))
  plot(h2,col=rgb(1,0,0,1/4),add=T)
  legend("topright", c("Before", "After", "Overlap"), 
         col=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4), rgb(0.5,0,0.5,1/4)), lwd=10)
}




ChangeTable <- function(old=plots, new=plotsNew){
  
  #AGB bins classes
  bins <- c(0,50,100,150,200,250,300,Inf) #7 intervals
  bins.str <-c('0-50','050-100','100-150','150-200','200-250','250-300', '300_above')
  
  #assign bins per plot row
  old1 <- transform(old, group=cut(AGB_T_HA,  breaks=bins))
  new1 <- transform(new, group=cut(AGB_T_HA,  breaks=bins))
  
  old2 <- old1 %>% 
    group_by(group) %>%
    tally()
  old2 <- as.data.frame(old2)
  new2 <- new1 %>% 
    group_by(group) %>%
    tally()
  new2 <- as.data.frame(new2)
  
  #summarize change in plot bins
  both <- cbind(old2[-8,],new2[,-1])
  both$change <- round(((both[,3] - both[,2] ) / both[,2] ) * 100, 2)
  both[,1] <- bins.str
  names(both) <- c('agb_bins', 'pre_TF', 'post_TF', '%change')
  print(both)
  
  #export table
  setwd(outDir)
  write.csv(both, paste0('pre_post_change_', Sys.Date(),'.csv'), row.names = F)
  setwd(mainDir)
  

  return(both)
}
