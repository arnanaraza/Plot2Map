### FUNCTION TO ASSIGN STRATA TO CIRCULAR PLOTS AND 
### COMPUTE WEIGHTED AGB BASED ON STRATA SIZE


StrataAGB <- function(plt=plots){
  plots$wt <- c(.22,.22,.68,.68,.68)
  wm <- weighted.mean(plots$AGB_T_HA, plots$wt)
  wsd <- weighted.mean(plots$sdTree, plots$wt)
  
  print(paste("plot AGB is", round(wm,2), 
              "with SD", round(wsd,2)))
  
}
