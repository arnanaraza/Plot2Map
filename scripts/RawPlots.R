### FUNCTION TO FORMAT PLOTS

RawPlots <- function(plots=plots, mapYear=mapYear){
  if(is.data.frame(plots) == F){
    stop('input file should be a data frame with XY coordinates')
  }
  
  id <-plots [,menu(names(plots), title="which column is your unique Plot ID?")]
  agb <- as.numeric(plots [,menu(names(plots), 
                                      title="which column is your plot AGB?")])
  x <-as.numeric(plots [,menu(names(plots), 
                                   title="put longitude  column")])
  y <-as.numeric(plots [,menu(names(plots), 
                              title="which column is your latitude")])
  size <- as.numeric(plots [,menu(names(plots), 
                                  title="plot size column")])
  fez <- NA
  gez <- NA
  year <-as.numeric(plots [,menu(names(plots), 
                                title=" year  column")])
  
  

  plt <- data.frame(id,x,y, agb, size, fez, gez, year)
  names(plt) <- c('PLOT_ID', 'POINT_X', 'POINT_Y', 'AGB_T_HA','SIZE_HA',
                  'FEZ', 'GEZ', 'AVG_YEAR')
  plt
}
