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
  size <- ifelse(size > 50, size/10000, size) #check if m2, if yes convert to ha
  fez <- NA
  gez <- NA
  year <-as.numeric(plots [,menu(names(plots), 
                                title=" year  column")])
  
  

  plt <- data.frame(id,x,y, agb, size, fez, gez, year)
  names(plt) <- c('PLOT_ID', 'POINT_X', 'POINT_Y', 'AGB_T_HA','SIZE_HA',
                  'FEZ', 'GEZ', 'AVG_YEAR')
  
  
  plt
}


RawPlotsTree <- function(plots=plots){
  if(is.data.frame(plots) == F){
    stop('input file should be a data frame with XY coordinates')
  }
  
  id <-plots [,menu(names(plots), title="which column is your unique Plot ID?")]
  genus <-plots [,menu(names(plots), title="column of tree Genus?")]
  
  species <-plots [,menu(names(plots), title="column of tree Species?")]
  
  diameter <-as.numeric(plots [,menu(names(plots), title="column of tree DBH?")])
  
  ans <- menu(c('yes', 'no'))
  
  if (ans=='yes'){
    height <-as.numeric(plots [,menu(names(plots), title="column of tree Height?")])
  }
  
  
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


  plt <- data.frame(id,	genus	,species,diameter, size, fez, gez, year)
  
  if (ans=='yes'){
    plt <- data.frame(id,	genus	,species,diameter,height, size, fez, gez, year)
  }
  
  plt1 <- data.frame(id,	x,y)
  
  
  plt$id <- factor(plt$id, levels=unique(plt$id), labels=seq_along(nrow(plt)))
  plt1$id <- factor(plt1$id, levels=unique(plt1$id), labels=seq_along(nrow(plt1)))
  
  
  list(plt,plt1)
  
}
