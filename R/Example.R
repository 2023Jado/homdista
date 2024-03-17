file <- read.csv("C:/Users/Jado/Documents/EAGLE/Semester 2/Data/African elephant Jammes Hwange NP2.csv", header=T)
tf <- "%m/%d/%y %H:%M"
crs_epsg <- 32734
groupid <- file$Animal
perc <- 95





library(sp)
library(sf)
library(ade4)
library(adehabitatMA)
library(CircStats)
library(adehabitatLT)
library(adehabitatHR)
library(move)
library(lubridate)
library(amt)
library(ggplot2)
library(scales)
library(mapview)
library(circular)
library(basemaps)
library(raster)
library(tidyr)

# Read the csv data

hom <- function(file, tf, crs_epsg, groupid, perc){
  data_df <- file
  # Change the time format
  data_df$time <- as.POSIXct(data_df$timestamp, format = tf, tz="UTC")

  # Remove the NA from data_df
  data_df_no_na <- na.omit(data_df)

  # Sort the dataset based on the timestamp column
  no_na_df_sorted <- data_df_no_na[order(data_df_no_na$time), ]

  # Identify duplicate timestamps
  duplicate_indices <- duplicated(no_na_df_sorted$time) | #If I don't do this, why do I get the error that dataset includes double timestamps?
    duplicated(no_na_df_sorted$time, fromLast = TRUE)     #I loose many rows if I apply this (however, it is the only option I have so far)

  # Remove duplicate timestamps
  no_na_data_unique <- no_na_df_sorted[!duplicate_indices, ]

  # Create a "code name" column to be used for home range estimation
  no_na_data_unique$Month_code <- month(no_na_data_unique$time)
  no_na_data_unique$Year_code <- year(no_na_data_unique$time)
  no_na_data_unique$Code <- paste(no_na_data_unique$Month_code, no_na_data_unique$Year_code, no_na_data_unique$Animal)

  # Create move object with sorted dataset
  df_move <- move(
    x = no_na_data_unique$x,
    y = no_na_data_unique$y,
    time = as.POSIXct(no_na_data_unique$time, format = "%m/%d/%y %H:%M", tz = "UTC"),
    data = no_na_data_unique,
    Id = na_na_data_unique$Animal,
    group = no_na_data_unique$Code,
    crs = 32734
  )

  # Assign the projection to the move object
  epsg_code <- 32734
  crs <- CRS(paste0("+init=epsg:", 32734))
  proj4string(df_move) <- crs

  ############################ Calculations of home range ##################################################

  # Calculate the bandwidth parameter from the move object using "amt package"
  df_move_track <- amt::make_track(df_move, x, y, time, crs=32734)
  parh <- as.numeric(amt::hr_kde_ref(df_move_track)[1])

  # Initialize a list to store KDE results for each unique name
  kde_list <- list()

  # Iterate over each unique name in the "Code" column
  unique_names <- unique(df_move$Code)
  for (name in unique_names) {

    # Subset the data for the current name
    subset_data <- df_move[df_move$Code == name, ]

    # Check the number of relocations
    num_relocations <- nrow(subset_data)

    # Calculate KDE only if there are at least 5 relocations
    if (num_relocations >= 5) {
      kde <- kernelUD(as(subset_data, "SpatialPoints"), h = parh)
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
      getverticeshr(kde, percent = 95, unout = "km2")
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
  home <- do.call(rbind, vertices_list)

  # Assign the projection to the calculated homerange
  proj4string(home) <- crs

  # Homerange as data frame
  home1 <- as.data.frame(home)
  home2 <- home1[, c("Code", "area")]

  home3 <- tidyr::separate(home2, Code, into = c("Month", "Year", "Id"), sep = " ")

  # Convert back to SpatialPolygonsDataFrame
  home3_sp <- SpatialPolygonsDataFrame(home, home3)

  # #################### Compute traveled distance ####################

  # Prepare the layer to be used

  coordinates <- df_move[, c("x", "y")]
  coordinates_sf <- st_as_sf(coordinates, crs=32734)
  df_move_df <- coordinates_sf #only changed the name of the layer
  df_move_df$time <- df_move$time
  df_move_df$Code <- df_move$Code

  # Sort df_move_df by timestamp
  df_move_sorted <- df_move_df[order(df_move$time), ]

  # Check for unique values in the 'Code' column
  unique_codes <- unique(df_move_sorted$Code)

  # Check if there are any missing or empty values in 'Code'column
  missing_codes <- is.na(unique_codes) | unique_codes == ""
  if (any(missing_codes)) {
    stop("Some code names are missing or empty.")
  }

  # Calculate traveled distance for each "Code" name
  traveled_distances <- lapply(split(coordinates_sf, df_move_sorted$Code), function(group_coords) {
    if (nrow(group_coords) > 1) {

      # Calculate distance between consecutive points
      distances <- st_distance(group_coords)

      # Sum the distances in km
      total_distance <- sum(distances)/1000
      return(total_distance)
    } else {
      return(0)  # Return 0 if there's only one or zero points
    }
  })

  # Convert the list of distances into a data frame
  traveled_distances_df <- do.call(rbind, lapply(names(traveled_distances), function(code_dista) {
    data.frame(Code = code_dista, Distance_km = traveled_distances[[code_dista]], row.names = NULL)
  }))

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




