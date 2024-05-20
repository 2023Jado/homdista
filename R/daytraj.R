#' Daily traveled distance
#' @author Jean de Dieu Tuyizere
#'
#' Connects all GPS points in the order of timestamps and computes the length of the distance per day.
#'
#' Arguments
#' @param file R-imported dataframe which comprises at least three columns: a longitude column labeled "x", a latitude column labeled "y", and a timestamp column labeled "timestamp", in lowercase.
#' @param tf timestamp format
#' @param crs_epsg the epsg code related to the dataset coordinates
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#'
#' @return Daily movement paths
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", "data.csv", package = "homdista")
#' file <- read.csv(file_path, header=T)
#'
#' # Define some parameters
#' tf <- "%m/%d/%y %I:%M %p"
#' Id_name <- "Animal"
#' crs_epsg <- 32734
#' perc <- 95
#'
#'
#' library(homdista)
#'
#' # Spatial lines (paths) showing daily traveled distance
#' distance_paths <- daytraj(file, tf, crs_epsg, Id_name)
#' head(distance_paths)
#' @import sp
#' @import sf
#' @import lubridate
#' @import tidyr

daytraj <- function(file, tf, crs_epsg, Id_name){

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
  no_na_df_sorted$Day_code <- day(no_na_df_sorted$time)
  no_na_df_sorted$Month_code <- month(no_na_df_sorted$time)
  no_na_df_sorted$Year_code <- year(no_na_df_sorted$time)
  no_na_df_sorted$Code <- paste(no_na_df_sorted$Day_code, no_na_df_sorted$Month_code, no_na_df_sorted$Year_code, no_na_df_sorted$groupid)

  # Change the data frame to "sf" object
  df_move <- st_as_sf(no_na_df_sorted, coords = c("x", "y"), crs=crs_epsg)

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

  # Merge the computed distances with the spatial data based on the 'Code' column
  distamove <- merge(df_move_sorted, traveled_distances_df, by = "Code")

  # Initialize an empty list to store paths and associated code name
  lines_list <- list()
  codes <- character(0)

  # Loop through each code name
  for (code in unique(distamove$Code)) {

    group_df <- distamove[distamove$Code == code, ]
    if (nrow(group_df) > 1) {
      line <- st_cast(st_union(st_cast(group_df, "MULTIPOINT")), "LINESTRING")
      lines_list[[code]] <- line
      codes <- c(codes, code)  # Add code to codes vector
    } else {
      lines_list[[code]] <- NA  # Indicate missing lines
    }
  }

  # Filter out NA values from codes and lines_list
  codes <- codes[!is.na(lines_list)]
  lines_list <- lines_list[!is.na(lines_list)]

  # Create a data frame with codes and corresponding lines
  lines_df <- data.frame(Code = rep(codes, sapply(lines_list, length)),
                         geometry = do.call("c", lines_list),
                         row.names = NULL)


  # Convert to sf object
  movement <- st_as_sf(lines_df)

  # Split the column of "Code" into month, year and Id
  movementsplit <- tidyr::separate(movement, Code, into = c("Day", "Month", "Year", "Id"), sep = " ")
  movementsplit$Distance_km <- paste(traveled_distances_df$Distance_km)
  movementsplit$Length_km <- st_length(movementsplit)/1000
  movementsplit$Length_km <- gsub("\\s*\\[m\\]", "", movementsplit$Length_km)
  Movementpath <- movementsplit[, c("Day", "Month", "Year", "Id", "Length_km")]
  head(Movementpath)

  return(Movementpath)
}

