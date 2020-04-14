
TempApply <- function(df, domain, year){
  gr <- read.csv(paste0(dataDir,'/GR_Uniques.csv'))
  gr$GEZ <- as.character(gr$GEZ)
  gr$ZONE <- as.character(gr$ZONE)
  gr$FAO.ecozone <- as.character(gr$FAO.ecozone)
  
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
  
  #remove extra column of baseline agb
  below <- below[ , !(names(below) %in% 'AGB_ORIG')]
  above <- above[ , !(names(above) %in% 'AGB_ORIG')]
  
  
  #combine all: static and recomputed
  df.new <- rbind(below,above,static)

  #checker of rows
  if (sum(nrow(df.old)) == sum(nrow(df.new))) {
    print ('growth rates applied correspondingly per eco-region!')}
  else {print('something is wrong..row sums not equal..')}
  
  #remove last joined growth rates columns for further row binding 
  remove <- c('GR1', 'GR2', 'GR3')
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
  below$AGB_ORIG <- below$AGB_T_HA
  above$AGB_ORIG <- above$AGB_T_HA
  below$AGB_T_HA <- below$AGB_T_HA + (ifelse(below$AGB_T_HA < 100, below$SD3, below$SD2) * (year - below$AVG_YEAR))  
  above$AGB_T_HA <- above$AGB_T_HA - (ifelse(above$AGB_T_HA < 100, above$SD3, above$SD2) * (above$AVG_YEAR -  year))  
  
  below$AGB_T_HA <- ifelse(below$AGB_T_HA > 152, 
                           below$AGB_ORIG + (below$SD1 * (year - below$AVG_YEAR)), below$AGB_T_HA)
  
  above$AGB_T_HA <- ifelse(above$AGB_T_HA > 152, 
                           above$AGB_ORIG - (above$SD1 * (above$AVG_YEAR - year)), above$AGB_T_HA) #retain if not in primary/SD3 class 
  
  above$AGB_T_HA <- ifelse(above$AGB_T_HA < 0, 
                           above$AGB_ORIG, above$AGB_T_HA) #retain original if it gets negative
  
  #remove extra column of baseline agb
  below <- below[ , !(names(below) %in% 'AGB_ORIG')]
  above <- above[ , !(names(above) %in% 'AGB_ORIG')]
  
  
  #combine all: static and recomputed
  df.new <- rbind(below,above,static)
  
  
  #checker of rows
  if (sum(nrow(df.old)) == sum(nrow(df.new))) {
    print ('growth rates applied correspondingly per eco-region!')}
  else {print('something is wrong..row sums not equal..')}
  
  #retain last joined growth rates columns for further row binding 
  retain <- c('POINT_X', 'POINT_Y', 'AGB_T_HA')
  df.new <- df.new[ , (names(df.new) %in% retain)]
  names(df.new) <- c('POINT_X', 'POINT_Y', 'SD')

  return(df.new)
}

