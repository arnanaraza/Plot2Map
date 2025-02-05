RefLidar <- function(lidar.dir = 'D:/AGBC/data/SustainableLandscapeBrazil_v03/SLB_AGBmaps', year = NA) {
  newproj <- "+proj=longlat +datum=WGS84"
  raw <- list.files(lidar.dir, full.names = TRUE)
  
  # Prompt for raster type
  raster_type <- readline(prompt = "Enter raster type (AGB, CV, or SD): ")
  raster_type <- toupper(raster_type)  # Ensure case consistency
  
  if (!raster_type %in% c("AGB", "CV", "SD")) {
    stop("Invalid raster type. Please enter 'AGB', 'CV', or 'SD'.")
  }
  
  # Load raster files
  r.files <- lapply(raw, function(x) {
    r <- raster(x)
    names(r) <- "value"  # Standardize the column name for raster data
    r
  })
  
  # Reproject to WGS84 if needed
  if (any(sapply(r.files, function(x) grepl('utm|meters|metre|UTM|zone|NAD', crs(x), ignore.case = TRUE)))) {
    r.files <- lapply(r.files, function(x) projectRaster(x, crs = newproj, method = 'bilinear'))
  }
  
  ha <- xres(r.files[[1]]) * 1000
  
  # Convert pixels to points
  pts.list <- lapply(r.files, function(x) {
    df <- as.data.frame(rasterToPoints(x))  # Convert raster to data frame
    colnames(df) <- c("POINT_X", "POINT_Y", raster_type)  # Rename columns
    df
  })
  
  # Add ID for each raster file
  pts.list <- lapply(seq_along(pts.list), function(i) {
    pts <- pts.list[[i]]
    pts$ID <- basename(raw[i])  # Add file name as ID
    pts
  })
  
  # Combine all points into a single data frame
  pts <- do.call(rbind, pts.list)
  
  # Manual entry for PLOT_ID
  print(pts$ID[1])
  plot_start <- as.integer(readline(prompt = "Enter index of the first letter of PLOT_ID: "))
  plot_end <- as.integer(readline(prompt = "Enter index of the last letter of PLOT_ID: "))
  pts$PLOT_ID <- substr(pts$ID, plot_start, plot_end)
  
  # If year is NA, ask the user for it
  if (is.na(year)) {
    print(pts$ID[1])
    year_start <- readline(prompt = "Enter index of the first letter of YEAR (leave blank if year is not in filename): ")
    
    if (year_start == "") {
      extracted_year <- readline(prompt = "Year not found in filename. Please enter YEAR manually: ")
    } else {
      year_start <- as.integer(year_start)
      year_end <- as.integer(readline(prompt = "Enter index of the last letter of YEAR: "))
      extracted_year <- substr(pts$ID, year_start, year_end)
    }
    
    # Convert two-digit years to four-digit format if necessary
    if (nchar(extracted_year) == 2) {
      extracted_year <- paste0("20", extracted_year)  # Assuming all two-digit years refer to the 21st century
    }
    
  } else {
    extracted_year <- as.character(year)  # Use the provided year
  }
  
  pts$AVG_YEAR <- extracted_year
  
  # Add additional columns
  pts$SIZE_HA <- ha
  
  # Format output based on raster type
  if (raster_type == "AGB") {
    pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "AGB", "AVG_YEAR")]
  } else if (raster_type == "CV") {
    pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "CV", "AVG_YEAR")]
  } else if (raster_type == "SD") {
    pts <- pts[, c("PLOT_ID", "POINT_X", "POINT_Y", "SD", "AVG_YEAR")]
  }
  
  return(pts)
}
