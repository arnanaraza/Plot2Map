

Deforested <- function(plt, fdir, map_year = 10) {
  
  # Ensure POINT_X and POINT_Y columns exist
  if (!"POINT_X" %in% colnames(plt)) {
    plt$POINT_X <- plt$Xnew
    plt$POINT_Y <- plt$Ynew
    plt$PLOT_ID <- 1:nrow(plt)
  }
  
  # Convert to sf object if it's a dataframe
  if (!inherits(plt, "sf")) {
    plt_sf <- st_as_sf(plt, coords = c("POINT_X", "POINT_Y"), crs = 4326)
  } else {
    plt_sf <- plt
  }
  
  defo <- numeric(nrow(plt_sf))  # Initialize deforestation vector
  
  for (p in seq_len(nrow(plt_sf))) {
    
    # Create square buffer polygon around each plot
    xy <- st_geometry(plt_sf[p, ])
    
    # Convert hectares to meters for buffer distance
    side_length_m <- sqrt(plt_sf$SIZE_HA[p] * 10000)  # Convert ha to square meters
    ww <- ifelse(!is.na(side_length_m), side_length_m / 2, 20)  # Default buffer (20m) if missing
    
    pol <- st_buffer(xy, dist = ww)  # Create buffer around point
    
    # **Fix: Ensure `pol` is in EPSG:4326 to match GFC**
    pol <- st_transform(pol, crs = 4326)  
    
    # Print the area of the plot for reference
    plot_area <- as.numeric(st_area(pol)) / 10000  # Convert m² to hectares
    print(paste('Processing:', round(plot_area, 2), 'ha'))  
    
    # Calculate bounding box and get tiles
    bb <- st_bbox(pol)
    gfcTile <- calc_gfc_tiles(pol)
    download_tiles(gfcTile, fdir, images = "lossyear", dataset = 'GFC-2022-v1.10')
    
    # Extract raster values from downloaded lossyear data using terra
    fnms <- list.files(fdir, pattern = "Hansen_GFC-2022-v1.10_lossyear_.*.tif", full.names = TRUE)
    vls <- numeric()
    
    for (f in fnms) {
      if (file.exists(f)) {
        loss_raster <- rast(f)  # Use terra's rast function
        extracted_values <- extract(loss_raster, vect(pol))[[1]]  # Use terra's extract
        vls <- c(vls, extracted_values)
      }
    }
    
    # Process the extracted values: deforestation is indicated by values > 0
    vls <- ifelse(length(vls[vls == 0]) > length(vls[vls > 0]), vls * 0, vls)
    vls[vls > 0] <- 1
    defo[p] <- sum(vls[vls > 0], na.rm = TRUE)
    print(defo[p])
  }
  
  # Assign deforestation values to the original data
  plt$defo <- defo
  thresh <- plt$SIZE_HA * 0.05  # Threshold is 5% of plot size in hectares
  print(paste('Removed', nrow(subset(plt, plt$defo > thresh)), 'plots that have >5% change'))
  
  # Filter out deforested plots based on threshold and map year
  defPlt <- subset(plt, plt$defo > 0) 
  defPlt <- subset(defPlt, defPlt$defo <= map_year)  # Filter for map year
  
  # Convert `sf` back to dataframe and remove geometry
  plt <- st_drop_geometry(plt)  # Removes geometry
  
  # Ensure POINT_X and POINT_Y are restored
  plt$POINT_X <- st_coordinates(plt_sf)[, 1]
  plt$POINT_Y <- st_coordinates(plt_sf)[, 2]
  
  # Ensure netPlt retains original format and columns
  netPlt <- plt %>% 
    filter(!PLOT_ID %in% defPlt$PLOT_ID) %>% 
    dplyr::select(-defo)  # Remove defo column to match input format
  
  # Ensure netPlt is a dataframe
  netPlt <- as.data.frame(netPlt)
  
  return(list(netPlt = netPlt, original = plt))
}
