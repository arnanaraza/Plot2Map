
TempApply <- function(df, domain, year){
  gr <- read.csv(paste0(dataDir,'/GR_Uniques.csv'))
  gr$GEZ <- as.character(gr$GEZ)
  gr$ZONE <- as.character(gr$ZONE)
  gr$FAO.ecozone <- as.character(gr$FAO.ecozone)
  
  #filter +- 10 years older or newer plots to the map year
  df <- subset(df, df$AVG_YEAR < year+10 
               & df$AVG_YEAR > year-10)
  
  #filter eco-region first
  df0 <- filter (df, GEZ == domain) #this part doesn't give NAs already
  
  #join growth rate table using 3 variables to assure uniqueness
  df.old <- left_join(df0, gr, by = c('GEZ'='GEZ', 'ZONE'='ZONE', 'FAO.ecozone'='FAO.ecozone')) 

  #filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
  below <- subset(df.old, AVG_YEAR < year) #non-NAs
  above <- subset(df.old, AVG_YEAR > year) #non-NAs
  static <- subset(df.old, is.na(AVG_YEAR) | AVG_YEAR == year) #NAs AVG_YEAR OR 2010 subsets
  
  
  #apply growth rates (GR1 = primary, GR2 = old secondary, GR3 = young secondary)
  below$AGB_ORIG <- below$AGB_T_HA
  above$AGB_ORIG <- above$AGB_T_HA
  below$AGB_T_HA <- below$AGB_T_HA + (ifelse(below$AGB_T_HA < 100, below$GR3, below$GR2) * (year - below$AVG_YEAR))  
  above$AGB_T_HA <- above$AGB_T_HA - (ifelse(above$AGB_T_HA < 100, above$GR3, above$GR2) * (above$AVG_YEAR -  year))  
  
  below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152, 
                           below$AGB_ORIG + (below$GR1 * (year - below$AVG_YEAR)), below$AGB_T_HA)
  
  above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152, 
                           above$AGB_ORIG - (above$GR1 * (above$AVG_YEAR - year)), above$AGB_T_HA) #retain if not in primary/GR3 class 
  
  above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0, 
                           above$AGB_ORIG, above$AGB_T_HA) #retain original if it gets negative
  

  #combine all: static and recomputed
  static$AGB_ORIG <- static$AGB_T_HA

  df.new <- rbind(below,above,static)
  df.new$AGB_T_HA_ORIG <- df.new$AGB_ORIG
  
  #checker of rows
  if (sum(nrow(df.old)) == sum(nrow(df.new))) {
    print ('growth rates applied correspondingly per eco-region!')}
  else {print('something is wrong..row sums not equal..')}
  
  #remove last joined growth rates columns for further row binding 
  remove <- c('GR1', 'GR2', 'GR3', 'AGB_ORIG')
  df.new <- df.new[ , !(names(df.new) %in% remove)]
  
  df.new$AGB_T_HA <- ifelse(is.na(df.new$AGB_T_HA),df$AGB_T_HA, df.new$AGB_T_HA)
  return(df.new)
}




TempVar <- function(df, domain, year){
  gr <- read.csv(paste0(dataDir,'/GR_SD.csv'))
  gr$GEZ <- as.character(gr$GEZ)
  gr$ZONE <- as.character(gr$ZONE)
  gr$FAO.ecozone <- as.character(gr$FAO.ecozone)
  
  #filter eco-region first
  df0 <- filter(df, GEZ == domain) #this part doesn't give NAs already
  
  #join growth rate table using 3 variables to assure uniqueness
  df.old <- left_join(df0, gr, by = c('GEZ'='GEZ', 'ZONE'='ZONE', 'FAO.ecozone'='FAO.ecozone')) 
  
  #filter above and below map year (i.e. 2010 for GlobBiomass), keep no changes to map year
  below <- subset(df.old, AVG_YEAR < year) #non-NAs
  above <- subset(df.old, AVG_YEAR > year) #non-NAs
  static <- subset(df.old, is.na(AVG_YEAR) | AVG_YEAR == year) #NAs AVG_YEAR OR 2010 subsets
  
  
  #apply growth rates (SD1 = primary, SD2 = old secondary, SD3 = young secondary)

  below$AGB_T_HA <- below$AGB_T_HA_ORIG + (ifelse(below$AGB_T_HA_ORIG < 100, below$SD3, below$SD2) * (year - below$AVG_YEAR))  
  above$AGB_T_HA <- above$AGB_T_HA_ORIG - (ifelse(above$AGB_T_HA_ORIG < 100, above$SD3, above$SD2) * (above$AVG_YEAR -  year))  
  
  below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152, 
                           below$AGB_T_HA_ORIG + (below$SD1 * (year - below$AVG_YEAR)), below$AGB_T_HA)
  
  above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152, 
                           above$AGB_T_HA_ORIG - (above$SD1 * (above$AVG_YEAR - year)), above$AGB_T_HA) #retain if not in primary/SD3 class 
  
  above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0, 
                           above$AGB_T_HA_ORIG, above$AGB_T_HA) #retain original if it gets negative
  
  
  #combine all: static and recomputed
  df.new <- rbind(below,above,static)
  
  
  #checker of rows
  if (sum(nrow(df.old)) == sum(nrow(df.new))) {
    print ('growth rates applied correspondingly per eco-region!')}
  else {print('something is wrong..row sums not equal..')}
  
  #retain last joined growth rates columns for further row binding 
  df.new$sdGrowth <-  abs(df.new$AGB_T_HA - df.new$AGB_T_HA_ORIG)
  retain <- c(names(df0), 'sdGrowth')
  df.new <- df.new[ , (names(df.new) %in% retain)]
  df.new$AGB_T_HA <-  df.old$AGB_T_HA
  df.new$sdGrowth <- ifelse(is.na(df.new$sdGrowth), mean(df.new$sdGrowth,na.rm=T),df.new$sdGrowth)
  return(df.new)
}

