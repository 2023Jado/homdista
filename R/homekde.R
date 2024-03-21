#' Home range spatial polygons
#'
#' @param file data frame file already read in R and has at least three columns named as follows longitude column named as "x", latitude column named as "y", and timestamp named as "timestamp"
#' @param tf timestamp format
#' @param crs_epsg the epsg code related to the dataset coordinates
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#' @param perc Percentage which is used to compute the home range utilization i.e kernel density estimation at a given level (percentage) (50% for core areas, 75%, 90%, 95%, ...)
#' @param parh bandwidth or smoothing parameter
#' @return home range polygons
#' @export
#'
#' @examples
#' file <- read.csv("C:/Users/Jado/Documents/EAGLE/Semester 2/Data/African elephant Jammes Hwange NP2.csv", header=T)
#'
#' # Define some parameters
#' tf <- "%m/%d/%y %H:%M"
#' Id_name <- "Animal"
#' crs_epsg <- 32734
#' perc <- 95
#'
#' library(homdista)
#'
#' # Home range spatial polygons
#' homerange_polygons <- homdista::homekde(file, tf, crs_epsg, Id_name, perc)
#'
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

  # Initialize a list to store KDE results for each unique name
  kde_list <- list()

  # Get unique names from "df_move$Code"
  unique_names <- unique(df_move$Code)

  # Loop through each unique "code name"
  for (name in unique_names) {

    # Subset the data for the current name
    subset_data <- df_move[df_move$Code == name, ]

    # Check the number of relocations
    num_relocations <- nrow(subset_data)

    # Calculate KDE only if there are at least 5 relocations
    if (num_relocations >= 5) {
      # Convert subset_data to SpatialPointsDataFrame
      subset_sp <- as(subset_data, "Spatial")

      # Calculate kernel UD
      kde <- kernelUD(subset_sp, h = parh)
      kde_list[[name]] <- kde
    } else {
      cat("Skipping kde calculation for", name, "due to fewer than 5 relocations.\n")
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
  for (i in 1:length(kde_list)) {
    vertices <- get_vertices(kde_list[[i]])
    if (!is.null(vertices)) {

      # Add code name column to vertices data frame
      vertices$Code <- unique_names[i]
      vertices_list[[i]] <- vertices
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


  # ###################### Plot a home range map ####################################

  # Convert "sp" object to "sf"
  homerange_sf <- st_as_sf(homeshape)

  # Define a palette for colors
  palette <- rainbow(length(unique(homerange_sf$Id)))

  # Create map with mapview
  map <- mapview(homerange_sf, zcol = "Id", col.regions = palette,
                 legend = TRUE, legend.title = "", legend.values = unique(homerange_sf$Id))
  map

  return(homeshape)
}




