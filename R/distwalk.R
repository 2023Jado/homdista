#' Traveled distance
#'
#' @param file data frame file already read in R and has at least three columns named as follows
#' longitude column named as "x", latitude column named as "y", and timestamp named as "timestamp"
#' @param tf timestamp format
#' @param crs_epsg the epsg code related to the dataset coordinates
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#'
#' @return movement
#' @export
#'
#' @examples example
#'
distwalk <- function(file, tf, crs_epsg, Id_name){

  data_df <- file

  # Rename the column
  names(data_df)[which(names(data_df) == Id_name)] <- "groupid"

  # Change the time format
  data_df$time <- as.POSIXct(data_df$timestamp, format = tf, tz="UTC")

  # Remove the NA from data_df
  data_df_no_na <- na.omit(data_df)

  # Sort the dataset based on the timestamp column
  no_na_df_sorted <- data_df_no_na[order(data_df_no_na$time), ]

  # Identify duplicate timestamps
  duplicate_indices <- duplicated(no_na_df_sorted$time) |
    duplicated(no_na_df_sorted$time, fromLast = TRUE)

  # Remove duplicate timestamps
  no_na_data_unique <- no_na_df_sorted[!duplicate_indices, ]

  # Create a "code name" column to be used for home range estimation
  no_na_data_unique$Month_code <- month(no_na_data_unique$time)
  no_na_data_unique$Year_code <- year(no_na_data_unique$time)
  no_na_data_unique$Code <- paste(no_na_data_unique$Month_code, no_na_data_unique$Year_code, no_na_data_unique$groupid)

  # Create move object with sorted dataset
  df_move <- move(
    x = no_na_data_unique$x,
    y = no_na_data_unique$y,
    time = as.POSIXct(no_na_data_unique$time, format = tf, tz = "UTC"),
    data = no_na_data_unique,
    Id = na_na_data_unique$groupid,
    group = no_na_data_unique$Code,
    crs = crs_epsg
  )

  # Assign the projection to the move object
  epsg_code <- crs_epsg
  crs <- CRS(paste0("+init=epsg:", epsg_code))
  proj4string(df_move) <- crs

  # Prepare the layer to be used

  coordinates <- df_move[, c("x", "y")]
  coordinates_sf <- st_as_sf(coordinates, crs=crs_epsg)
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

  # Create a data frame with codes and corresponding lines
  lines_df <- data.frame(Code = rep(codes, sapply(lines_list, length)),
                         geometry = do.call("c", lines_list),
                         row.names = NULL)

  # Convert to sf object
  movement <- st_as_sf(lines_df)

  print(movement)

  return(movement)
}


