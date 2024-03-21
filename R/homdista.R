#' Estimation of kde habitat utilization area and traveled distance
#'
#' @param file data frame already read in R and has at least the following three columns:
#' longitude column named as "x", latitude column named as "y", and timestamp named as "timestamp" (both in lower case).
#' @param tf timestamp format
#' @param crs_epsg the epsg code related to the dataset coordinates
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#' @param perc Percentage which is used to compute the home range utilization i.e kernel density estimation at a given level (percentage) (50% for core areas, 75%, 90%, 95%, ...)
#' @param parh bandwidth or smoothing parameter
#' @return home range area in km2 and traveled distance in km
#' @export
#'
#' @examples
#' file <- read.csv("C:/Users/Jado/Documents/EAGLE/Semester 2/Data/African elephant Jammes Hwange NP2.csv", header=T)
#'
#' # Define some parameters
#'tf <- "%m/%d/%y %H:%M"
#'Id_name <- "Animal"
#'crs_epsg <- 32734
#'perc <- 95
#'
#'library(homdista)
#'
#'# Compute the area utilized and distance traveled by elephant
#'area_distance <- homdista::homdista(file, tf, crs_epsg, Id_name, perc)
#'
homdista <- function(file, tf, crs_epsg, Id_name, perc, parh){

  # List of packages
  packages <- c("sp", "sf", "ade4", "adehabitatMA",
                "CircStats", "adehabitatLT", "adehabitatHR",
                "lubridate", "ggplot2", "scales", "mapview",
                "circular", "basemaps", "tidyr")

  # Function to install and load packages

  install_load_packages <- function(packages) {

    # Check if packages are not installed
    to_be_install <- setdiff(packages, rownames(installed.packages()))

    if (length(to_be_install) > 0) {

      # Install missing packages
      install.packages(to_be_install)
    }

    # Load packages
    lapply(packages, require, character.only = TRUE)
  }

  # Install and load packages
  install_load_packages(packages)

  library(sp)
  library(sf)
  library(ade4)
  library(adehabitatMA)
  library(CircStats)
  library(adehabitatLT)
  library(adehabitatHR)
  library(lubridate)
  library(ggplot2)
  library(scales)
  library(mapview)
  library(circular)
  library(basemaps)
  library(tidyr)

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
  home3_sp <- SpatialPolygonsDataFrame(home, home3)

  # #################### Compute traveled distance ####################


  # Sort df_move by timestamp
  df_move_sorted <- df_move[order(df_move$time), ]

  # Check for unique values in the 'Code' column
  unique_codes <- unique(df_move_sorted$Code)

  # Check if there are any missing or empty values in 'Code'column
  missing_codes <- is.na(unique_codes) | unique_codes == ""
  if (any(missing_codes)) {
    stop("Some code names are missing or empty.")
  }

  # Initialize an empty list to store distances
  traveled_distances <- list()

  # Loop through each "Code" name
  for (code in unique_codes) {

    # Subset the data for the current code
    subset_data <- df_move_sorted[df_move_sorted$Code == code, ]

    # Check the number of relocations
    num_relocations <- nrow(subset_data)

    # Proceed if there are at least 5 relocations
    if (num_relocations >= 5) {

      # Calculate distance between consecutive points
      distances <- st_distance(subset_data)

      # Sum the distances in km
      total_distance <- sum(distances) / 1000

      # Store the distance for this code
      traveled_distances[[code]] <- total_distance
    } else {
      cat("Deleting subset for", code, "due to fewer than 5 relocations.\n")

      # Delete this subset from the dataset
      df_move_sorted <- df_move_sorted[df_move_sorted$Code != code, ]
    }
  }

  # Convert the list of distances into a data frame
  traveled_distances_df <- data.frame(
    Code = names(traveled_distances),
    Distance_km = unlist(traveled_distances),
    row.names = NULL
  )


  # Remove the "[m]" suffix from the "Distance_km" column
  traveled_distances_df$Distance_km <- gsub("\\s*\\[m\\]", "", traveled_distances_df$Distance_km)

  # ##################### Merge the homerange and distance columns #############################################

  merged_distance_homerange <- merge(traveled_distances_df, home2, by="Code")

  # Splitting the Code column into three separate columns
  merged_distance_homerange_split <- tidyr::separate(merged_distance_homerange, Code, into = c("Month", "Year", "Id"), sep = " ")

  # Change the name of area
  names(merged_distance_homerange_split) <- c("Month", "Year", "Id", "Distance_km", "Area_km2")

  # Final file ready for correlation analysis
  final_file <- merged_distance_homerange_split

  return(final_file)
}
