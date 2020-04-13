### FUNCTION TO CREATE POLYGONS FROM SUBPLOTS WITH CORNER COORDINATES IN METERS i.e. Labriere et al. 2018
### AND POSSIBLE IRREGULAR PLOTS (NON-RECTANGULAR AND NON-SQUARED)

Polygonize <- function(){
  
  
  
  
  
}
# Globals
dataDir <- 'D:/supersites'

# Open raw data and separate sites per UTM zone
tropTROP0 <- read.csv(paste0(dataDir,'/CSV_TropiSAR_AfriSAR/TropiSAR_plotbased_AGB.csv'), 
                      sep = ';') #French Guiana all

# Separate corner plot coordinates per site
tropTROP <- tropTROP0[,c(2,5:12)] #
names(tropTROP) <- c('c', 'x', 'y', 'x', 'y', 'x', 'y', 'x', 'y') #sw > se > ne > nw
id=unique(tropTROP$c)

# Function to create a polygon using corner plots
polyThis <- function(df, x){
  r <- rbind(df[x,c(2,3)],  df[x,c(6,7)], df[x,c(8,9)], df[x,c(4,5)]) #sw > se > ne > nw
  print(r)
   r$id = df[x,1]
 # p = Polygon(r)
#  ps = Polygons(list(p),1)
 # sps = SpatialPolygons(list(ps))
  return(r)
}

# Implement function per site and define default UTM coordinates
tropTROP= tropTROP[1:20,]
tropPoly <- lapply(1:nrow(tropTROP), function(x) polyThis(tropTROP, x))
tropPoly1 <- do.call(bind, tropPoly) 
centrTrop <- gCentroid(tropPoly1,byid=TRUE)
proj4string(centrTrop) <-CRS("+init=epsg:32622") #French Guina


# Reproject to decimal degrees
SRS <- CRS("+init=epsg:4326")
centrTrop1 <- spTransform(centrTrop, SRS)


# Attach other plot info
DFtrop <- data.frame(tropTROP0$Area_code, centrTrop1@coords, tropTROP0$agb_chv, tropTROP0$Scale, tropTROP0$Site)
names(DFtrop) <- c('area', 'x', 'y', 'agb', 'size', 'site')


# if irregular