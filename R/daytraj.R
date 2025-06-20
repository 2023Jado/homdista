#' Daily traveled distance
#' @author Jean de Dieu Tuyizere
#'
#' Connects all GPS points in the order of timestamps and computes the length of the distance per day.
#'
#' Arguments
#'
#' @param file R-imported dataframe which comprises at least three columns: a longitude column labeled "x", a latitude column labeled "y", and a timestamp column labeled "timestamp", in lowercase.
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#' @param timestamp timestamp Column name from dataset which shows the time of the observation.
#' @param crs_epsg the epsg code related to the dataset coordinates.
#'
#' @return Daily movement paths
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", "data.csv", package = "homdista")
#' file <- read.csv(file_path, header=T)
#'
#' # Define some parameters
#' timestamp <- "timestamp"
#' Id_name <- "Animal"
#' crs_epsg <- 32734
#' perc <- 95
#'
#'
#' library(homdista)
#'
#' # Spatial lines (paths) showing daily traveled distance
#' distance_paths <- daytraj(file, Id_name, timestamp, crs_epsg)
#' head(distance_paths)
#' @import sp
#' @import sf
#' @import lubridate
#' @import tidyr
#' @import dplyr
#' @import anytime

daytraj <- function(file, Id_name, timestamp, crs_epsg){

  # Read the csv data

  data_df <- file
  names(data_df)[which(names(data_df) == Id_name)] <- "groupid"
  names(data_df)[which(names(data_df) == timestamp)] <- "timestamp"

  if (!("timestamp" %in% names(data_df))) {
    stop("Error: 'timestamp' column not found in the data frame.")
  }

  timestamp_raw <- as.character(data_df$timestamp)
  sample_timestamps <- timestamp_raw[!is.na(timestamp_raw) & timestamp_raw != ""]

  if (length(sample_timestamps) == 0) {
    stop("Error: 'timestamp' column is empty or contains only NA values.")
  }

  # Automatically recognize and parse the format of timestamp
  formats_to_try <- c(
    "%m/%d/%y %H:%M", "%m/%d/%Y %H:%M", "%m/%d/%y %H:%M:%S", "%m/%d/%y", "%d/%m/%y", "%y-%m-%d",
    "%m-%d-%y", "%d-%m-%y", "%d-%b-%y", "%d-%b-%Y", "%d %b %y", "%d %b %Y",
    "%y-%m-%d %H:%M:%S", "%m-%d-%y %H:%M:%S", "%d-%m-%y %H:%M:%S", "%b-%d-%y", "%b-%d-%Y",
    "%d/%m/%y %H:%M:%S","%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%b %d %y", "%b %d %Y", "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y", "%d-%m-%Y", "%m/%d/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M:%S", "%m-%d-%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S", "%Y-%m-%d %I:%M:%S %p",
    "%m/%d/%Y %I:%M:%S %p", "%d/%m/%Y %I:%M:%S %p", "%y-%m-%d %H:%M:%S", "%d/%m/%y %H:%M:%S",
    "excel_serial"
  )

  parsed_time <- NULL
  matched_format <- NULL

  # Test for each format and find the one that works
  for (fmt in formats_to_try) {
    if (fmt == "excel_serial") {
      # Check if all timestamps are numeric and look like Excel serial dates
      numeric_vals <- suppressWarnings(as.numeric(timestamp_raw))
      if (!all(is.na(numeric_vals)) && any(numeric_vals > 25000, na.rm = TRUE)) {
        test_parsed <- as.POSIXct((numeric_vals - 25569) * 86400,
                                  origin = "1970-01-01", tz = "UTC")
        success_rate <- sum(!is.na(test_parsed)) / length(test_parsed)

        if (success_rate > 0.8) {
          parsed_time <- test_parsed
          matched_format <- fmt
          break
        }
      }
    } else {
      # Test the format on sample timestamps
      test_sample <- suppressWarnings(as.POSIXct(sample_timestamps, format = fmt, tz = "UTC"))
      sample_success_rate <- sum(!is.na(test_sample)) / length(sample_timestamps)

      if (sample_success_rate > 0.8) {
        # Apply the successful format to ALL timestamp data
        parsed_time <- suppressWarnings(as.POSIXct(timestamp_raw, format = fmt, tz = "UTC"))
        full_success_rate <- sum(!is.na(parsed_time)) / length(timestamp_raw)

        # Verify it works on the full dataset too
        if (full_success_rate > 0.8) {
          matched_format <- fmt
          break
        }
      }
    }
  }

  # If no standard format worked, try flexible parsing
  if (is.null(parsed_time) || sum(!is.na(parsed_time)) / length(timestamp_raw) <= 0.8) {
    if (requireNamespace("anytime", quietly = TRUE)) {
      parsed_time <- suppressWarnings(anytime::anytime(timestamp_raw, tz = "UTC"))
      matched_format <- "anytime_flexible"
    } else if (requireNamespace("lubridate", quietly = TRUE)) {
      parsed_time <- suppressWarnings(lubridate::parse_date_time(
        timestamp_raw,
        orders = c("ymd HMS", "mdy HMS", "dmy HMS", "ymd", "mdy", "dmy"),
        tz = "UTC"
      ))
      matched_format <- "lubridate_flexible"
    } else {
      stop("Unable to parse 'timestamp' column. Consider specifying format manually.")
    }
  }

  # Final check - ensure we have successfully parsed timestamps
  final_success_rate <- sum(!is.na(parsed_time)) / length(parsed_time)
  if (final_success_rate <= 0.8) {
    stop(paste("Timestamp parsing failed. Only",
               round(final_success_rate * 100, 1),
               "% of timestamps were successfully parsed."))
  }

  data_df$time <- parsed_time

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
    if (num_relocations >= 2) {

      # Calculate distance between consecutive points
      distances <- st_distance(subset_data)

      # Sum the distances in km
      total_distance <- sum(distances) / 1000

      # Store the distance for this code
      traveled_distances[[code]] <- total_distance
    } else {
      cat("Deleting subset for", code, "due to fewer than 2 relocations.\n")

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

