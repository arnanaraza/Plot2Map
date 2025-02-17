### FUNCTION TO FORMAT PLOTS
RawPlots <- function(plots, mapYear = NULL) {
  # Type checking
  if (!is.data.frame(plots)) {
    stop('Input file should be a data frame with XY coordinates')
  }
  
  # Helper function for column selection or manual entry
  select_column <- function(prompt) {
    # Exclude list columns (e.g., geometry in spatial data)
    valid_cols <- names(plots)[!sapply(plots, is.list)]
    choice <- menu(c("Manual entry", valid_cols), title = prompt)
    if (choice == 1) {
      manual_entry <- readline("Enter the numeric value for manual entry: ")
      return(as.numeric(manual_entry))
    } else {
      colname <- valid_cols[choice - 1]
      col_data <- plots[[colname]]
      # If the column isn’t numeric, try converting via character
      if (!is.numeric(col_data)) {
        col_data <- as.numeric(as.character(col_data))
      }
      return(col_data)
    }
  }
  
  id   <- select_column("Which column is your unique Plot ID?")
  agb  <- select_column("Which column is your plot AGB?")
  x    <- select_column("Select longitude column")
  y    <- select_column("Which column is your latitude?")
  size <- select_column("Select plot size column")
  year <- select_column("Select year column")
  
  # Convert sizes from m² to ha if size > 50 (assuming m² if large)
  size <- ifelse(!is.na(size) & size > 50, size / 10000, size)
  
  # Initialize unused columns
  fez <- gez <- NA
  
  # Create and format data frame
  plt <- data.frame(PLOT_ID  = id,
                    POINT_X  = x,
                    POINT_Y  = y,
                    AGB_T_HA = agb,
                    SIZE_HA  = size,
                    FEZ      = fez,
                    GEZ      = gez,
                    AVG_YEAR = year)
  
  # Filter rows where AGB is not NA
  plt <- subset(plt, !is.na(AGB_T_HA))
  return(plt)
}

### FUNCTION TO FORMAT TREE DATA
RawPlotsTree <- function(plots) {
  if (!is.data.frame(plots)) {
    stop('Input file should be a data frame with XY coordinates')
  }
  
  # Exclude list columns from selection
  valid_cols <- names(plots)[!sapply(plots, is.list)]
  
  # Helper function for simple column selection
  get_column <- function(prompt) {
    choice <- menu(valid_cols, title = prompt)
    colname <- valid_cols[choice]
    return(plots[[colname]])
  }
  
  id      <- get_column("Which column is your unique Plot ID?")
  genus   <- get_column("Column of tree Genus?")
  species <- get_column("Column of tree Species?")
  diameter <- as.numeric(get_column("Column of tree DBH?"))
  
  ans <- menu(c('yes', 'no'), title = "Is tree height data available?")
  if (ans == 1) {
    height <- as.numeric(get_column("Column of tree Height?"))
  }
  
  x    <- as.numeric(get_column("Column for longitude?"))
  y    <- as.numeric(get_column("Column for latitude?"))
  size <- as.numeric(get_column("Plot size column?"))
  fez  <- NA
  gez  <- NA
  year <- as.numeric(get_column("Year column?"))
  
  if (ans == 1) {
    plt <- data.frame(id, genus, species, diameter, height, size, fez, gez, year)
  } else {
    plt <- data.frame(id, genus, species, diameter, size, fez, gez, year)
  }
  
  plt1 <- data.frame(id, x, y)
  
  # Optionally recode the unique Plot ID as numeric factors
  plt$id  <- as.numeric(as.factor(plt$id))
  plt1$id <- as.numeric(as.factor(plt1$id))
  
  return(list(plt, plt1))
}
