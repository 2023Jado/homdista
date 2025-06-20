#' Yearly home range spatial polygons
#' @author Jean de Dieu Tuyizere
#'
#' Estimate the utilized home range size for each group/individual/etc..
#'
#' Arguments
#'
#' @param file R-imported dataframe which comprises at least three columns: a longitude column labeled "x", a latitude column labeled "y", and a timestamp column labeled "timestamp", in lowercase.
#' @param crs_epsg the epsg code related to the dataset coordinates
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#' @param timestamp timestamp Column name from dataset which shows the time of the observation.
#' @param perc The percentage utilized to calculate the KDE home range at a specific level (e.g., 50% for core areas, 75%, 90%, 95%, ...).
#' @param parh bandwidth or smoothing parameter
#'
#' @return home range polygons
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", "data.csv", package = "homdista")
#' file <- read.csv(file_path, header=T)
#'
#' # Define parameters
#' timestamp <- "timestamp"
#' Id_name <- "Animal"
#' crs_epsg <- 32734
#' perc <- 95
#' parh <- 500
#'
#' library(homdista)
#' #Additional libraries
#' library(sf)
#' library(mapview)
#'
#' # Yearly home range spatial polygons
#' homerange <- homeyear(file, crs_epsg, Id_name, timestamp, perc, parh)
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
#' @import adehabitatLT
#' @import adehabitatHR
#' @import lubridate
#' @import mapview
#' @import tidyr

homeyear <- function(file, crs_epsg, Id_name, timestamp, perc, parh){

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
    "%m/%d/%y %H:%M", "%m/%d/%y %H:%M:%S", "%m/%d/%y", "%d/%m/%y", "%y-%m-%d",
    "%m-%d-%y", "%d-%m-%y", "%d-%b-%y", "%d-%b-%Y", "%d %b %y", "%d %b %Y",
    "%y-%m-%d %H:%M:%S", "%m-%d-%y %H:%M:%S", "%d-%m-%y %H:%M:%S", "%b-%d-%y", "%b-%d-%Y",
    "%m/%d/%y %H:%M:%S", "%d/%m/%y %H:%M:%S","%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%b %d %y", "%b %d %Y",
    "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y", "%d-%m-%Y",
    "%m/%d/%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S", "%m-%d-%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
    "%Y-%m-%d %I:%M:%S %p", "%m/%d/%Y %I:%M:%S %p", "%d/%m/%Y %I:%M:%S %p",
    "%y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%d/%m/%y %H:%M:%S",
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
  no_na_df_sorted$Year_code <- year(no_na_df_sorted$time)
  no_na_df_sorted$Code <- paste(no_na_df_sorted$Year_code, no_na_df_sorted$groupid)

  # Change the data frame to "sf" object
  df_move <- st_as_sf(no_na_df_sorted, coords = c("x", "y"), crs=crs_epsg)

  ############################ Calculations of home range ##################################################

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

  home3 <- tidyr::separate(home2, Code, into = c("Year", "Id"), sep = " ")
  names(home3) <- c("Year", "Id", "area_km2")

  # Convert back to SpatialPolygonsDataFrame

  homeshape <- SpatialPolygonsDataFrame(home, home3)

  return(homeshape)
}
