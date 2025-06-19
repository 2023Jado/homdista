#' Data frame to Move object
#' @author Jean de Dieu Tuyizere
#'
#' Converts the data frame into a move object for further movement analysis.
#'
#' Arguments
#'
#' @param file R-imported dataframe which comprises at least three columns: a longitude column labeled "x", a latitude column labeled "y", and a timestamp column labeled "timestamp", in lowercase.
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#' @param timestamp timestamp Column name from dataset which shows the time of the observation.
#' @param crs_epsg the epsg code related to the dataset coordinates.
#'
#' @return move object
#' @export
#'
#' @examples
#'
#' file_path <- system.file("extdata", "data.csv", package = "homdista")
#' file <- read.csv(file_path, header=T)
#'
#' # Define parameters
#' timestamp <- "timestamp"
#' Id_name <- "Animal"
#' crs_epsg <- 32734
#' perc <- 95
#'
#'
#' library(homdista)
#' #Additional libraries for plotting
#' library(sp)
#' library(sf)
#' library(mapview)
#'
#' #Make the move object from data frame
#' Move <- moveObject(file, Id_name, timestamp, crs_epsg)
#' plot(Move)
#' mapview(Move)
#' @import sp
#' @import sf
#' @import lubridate
#' @import move
#' @import dplyr
#' @import anytime

moveObject <- function(file, Id_name, timestamp, crs_epsg){
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
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%m-%d-%Y", "%d-%m-%Y",
    "%m/%d/%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S", "%m-%d-%Y %H:%M:%S", "%d-%m-%Y %H:%M:%S",
    "%Y-%m-%d %I:%M:%S %p", "%m/%d/%Y %I:%M:%S %p", "%d/%m/%Y %I:%M:%S %p",
    "%y-%m-%d %H:%M:%S", "%m/%d/%y %H:%M:%S", "%d/%m/%y %H:%M:%S",
    "excel_serial"
  )

  parsed_time <- NULL
  for (fmt in formats_to_try) {
    if (fmt == "excel_serial") {
      numeric_vals <- suppressWarnings(as.numeric(data_df$timestamp))
      if (!all(is.na(numeric_vals)) && any(numeric_vals > 25000, na.rm = TRUE)) {
        parsed_time <- as.POSIXct((as.numeric(timestamp_raw) - 25569) * 86400,
                                  origin = "1970-01-01", tz = "UTC")
        if (sum(!is.na(parsed_time)) > length(parsed_time) * 0.8) {
          break
        }
      }
    } else {
      parsed_time <- suppressWarnings(as.POSIXct(sample_timestamps, format = fmt, tz = "UTC"))
      success_rate <- sum(!is.na(parsed_time)) / length(sample_timestamps)
      if (success_rate > 0.8) {
        break
      }
    }
  }

  if (is.null(parsed_time) || sum(!is.na(parsed_time)) / length(sample_timestamps) <= 0.8) {
    if (requireNamespace("anytime", quietly = TRUE)) {
      parsed_time <- suppressWarnings(anytime::anytime(data_df$timestamp, tz = "UTC"))
    } else if (requireNamespace("lubridate", quietly = TRUE)) {
      parsed_time <- suppressWarnings(lubridate::parse_date_time(
        timestamp_raw,
        orders = c("ymd HMS", "mdy HMS", "dmy HMS", "ymd", "mdy", "dmy"),
        tz = "UTC"
      ))
    } else {
      stop("Unable to parse 'timestamp' column. Consider specifying format manually.")
    }
  }

  data_df$time <- parsed_time

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

  ######## Change the projection ###########
  # Create a CRS object using the EPSG code
  crs_object <- CRS(paste0("+init=epsg:", crs_epsg))

  # Set the proj4string attribute of df_move to the CRS object
  proj4string(df_move) <- crs_object

  return(df_move)

}
