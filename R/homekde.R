#' Home range spatial polygons
#'
#' Arguments
#' @param file R-imported dataframe which comprises at least three columns: a longitude column labeled "x", a latitude column labeled "y", and a timestamp column labeled "timestamp", in lowercase.
#' @param tf timestamp format
#' @param crs_epsg the epsg code related to the dataset coordinates
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#' @param perc The percentage utilized to calculate the KDE home range at a specific level (e.g., 50% for core areas, 75%, 90%, 95%, ...).
#' @param parh bandwidth or smoothing parameter
#' @return home range polygons
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", "data.csv", package = "homdista")
#' file <- read.csv(file_path, header=T)
#'
#' # Define parameters
#' tf <- "%m/%d/%y %I:%M %p"
#' Id_name <- "Animal"
#' crs_epsg <- 32734
#' perc <- 95
#' parh <- 500
#'
#' library(homdista)
#' #Additional libraries
#' library(sf)
#' library(mapview)
#' library(webshot)
#'
#' # Home range spatial polygons
#' homerange <- homekde(file, tf, crs_epsg, Id_name, perc, parh)
#' homerange
#'
#' # Convert "sp" object to "sf"
#' homerange_sf <- st_as_sf(homerange)
#'
#'  plot(homerange_sf)
#'
#' # Define a palette for colors
#' palette <- rainbow(length(unique(homerange_sf$Id)))
#'
#' #Create map with mapview
#' mapview(homerange_sf, zcol = "Id", col.regions = palette, legend = TRUE, legend.title = "", legend.values = unique(homerange_sf$Id))
#' @import sp
#' @import sf
#' @import ade4
#' @import adehabitatMA
#' @import CircStats
#' @import adehabitatLT
#' @import adehabitatHR
#' @import lubridate
#' @import mapview
#' @import tidyr
#' @import webshot

homekde <- function(file, tf, crs_epsg, Id_name, perc, parh){

  # Read the csv data

  data_df <- file

  # Rename the column
  names(data_df)[which(names(data_df) == Id_name)] <- "groupid"

  # Change the time format
  data_df$time <- as.POSIXct(data_df$timestamp, format = tf, tz="UTC")

  # Remove the NA from data_df
  data_df_no_na <- na.omit(data_df)

  # Sort the dataset based on the timestamp column
  no_na_df_sorted <- data_df_no_na[order(data_df_no_na$time), ]

  # Create a "code name" column to be used for home range estimation
  no_na_df_sorted$Month_code <- month(no_na_df_sorted$time)
  no_na_df_sorted$Year_code <- year(no_na_df_sorted$time)
  no_na_df_sorted$Code <- paste(no_na_df_sorted$Month_code, no_na_df_sorted$Year_code, no_na_df_sorted$groupid)

  # Change the data frame to "sf" object
  df_move <- st_as_sf(no_na_df_sorted, coords = c("x", "y"), crs=crs_epsg)

  ############################ Calculations of home range ##################################################

  # Get unique names from df_move$Code
  unique_names <- unique(df_move$Code)

  # Initialize an empty list to store KDE results
  kde_list <- list()

  # Loop through each unique "code name"
  for (name in unique(df_move$Code)) {

    # Subset the data for the current code name
    subset_data <- df_move[df_move$Code == name, ]

    # Check the number of relocations
    num_relocations <- nrow(subset_data)

    # Proceed if there are at least 5 relocations
    if (num_relocations >= 5) {

      # Convert subset_data to SpatialPointsDataFrame
      subset_sp <- st_as_sf(subset_data, coords = c("x", "y"))

      # Convert subset_sp to SpatialPoints object
      subset_sp_points <- as(subset_sp, "Spatial")

      # Calculate kernel UD
      kde <- kernelUD(subset_sp_points, h = parh)
      kde_list[[name]] <- kde
    } else {
      cat("Deleting KDE result for", name, "due to fewer than 5 relocations.\n")

      # Delete this subset from the list
      kde_list[[name]] <- NULL
    }
  }

  # Get the vertices
  # Function to extract vertices for each "code" name stored in kde_list
  get_vertices <- function(kde) {

    # Extract vertices accounting for a certain percentage of the kernel density in an area unit
    code_name <- tryCatch({
      getverticeshr(kde, percent = perc, unout = "km2")
    }, error = function(e) {
      return(NULL)  # Return NULL if calculation fails
    })
    return(code_name)
  }

  # Create a list to store vertices for each "code" name stored in kde_list
  vertices_list <- list()

  # Iterate over each KDE object and extract vertices for each "code" name
  for (name in names(kde_list)) {
    vertices <- get_vertices(kde_list[[name]])
    if (!is.null(vertices)) {
      # Add code name column to vertices data frame
      vertices$Code <- name
      vertices_list[[name]] <- vertices
    }
  }

  # Combine all vertices into a single data frame
  # First of all, filter out NULL elements from vertices_list
  vertices_list_filtered <- vertices_list[!sapply(vertices_list, is.null)]

  # Check if the filtered list is not empty
  if (length(vertices_list_filtered) == 0) {
    stop("Error: vertices_list does not contain valid elements.")
  } else {
    # Second, create SpatialPolygons (all combined together)
    home <- do.call(rbind, vertices_list_filtered)
  }


  # Homerange as data frame
  home1 <- as.data.frame(home)
  home2 <- home1[, c("Code", "area")]

  home3 <- tidyr::separate(home2, Code, into = c("Month", "Year", "Id"), sep = " ")

  # Convert back to SpatialPolygonsDataFrame

  homeshape <- SpatialPolygonsDataFrame(home, home3)

  return(homeshape)
}




