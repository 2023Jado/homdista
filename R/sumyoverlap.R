
#' Summarizing yearly overlap statistics
#'
#' @param file dataframe which comprises at least three columns: a longitude column labeled "x", a latitude column labeled "y", in lowercase, and a timestamp column.
#' @param filelap this refers to the output of the `yearoverlap` function, which contains the overlap polygons and their attributes.
#' @param Id_name column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...)).
#' @param timestamp column name from filepoint which shows the time of the point (e.g., "timestamp", "time", "date", ...).
#' @param crs_epsg the epsg code related to the dataset coordinates.
#'
#' @return A layer summarizing the overlap statistics that can be visualized also in GIS software.
#' @export
#'
#' @examples
#' overlapp <- homoverlap(data, 32734)
#' summary_overlaps <- sumyoverlap(file, overlapp, "Animal", "timestamp", 37234)
#'
#' @import sf
#' @import dplyr
#' @import lubridate
#' @import anytime


sumyoverlap <- function(file, filelap, Id_name, timestamp, crs_epsg){
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
        # Apply the successful format to Aall timestamp data
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

  # Ensure successfully parsed timestamps
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

  # Change the data frame to "sf" object
  df_move <- st_as_sf(no_na_df_sorted, coords = c("x", "y"), crs=crs_epsg)


  overlapp <- filelap

  ovr_real <- overlapp %>%
    dplyr::filter(overlapped_with != "unoverlapped")

  # Group and union overlap polygons per Id-Year
  ovr_grouped <- ovr_real %>%
    dplyr::group_by(Id, Year) %>%
    dplyr::summarise(
      geometry = sf::st_union(geometry),
      area_km2 = dplyr::first(area_km2),
      total_overlapped_area_km2 = dplyr::first(total_overlapped_area_km2),
      unoverlapped_area_km2 = dplyr::first(unoverlapped_area_km2),
      .groups = "drop"
    )

  # Count number of unique overlap partners per Id-Year
  overlaps_count <- ovr_real %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct(Id, Year, overlapped_with) %>%
    dplyr::count(Id, Month, Year, name = "n_overlaps")

  # Count unique days with points inside overlap polygons
  count_days_for_id <- function(id, year, poly_geom) {
    df_move %>%
      dplyr::filter(groupid == id, Year_code == year) %>%
      sf::st_filter(poly_geom, .predicate = sf::st_within) %>%
      dplyr::mutate(date_only = as.Date(time)) %>%
      dplyr::distinct(date_only) %>%
      nrow()
  }

  # Apply day counting for each overlap polygon
  n_days_df <- ovr_grouped %>%
    dplyr::rowwise() %>%
    dplyr::mutate(n_days = count_days_for_id(Id, Year, geometry)) %>%
    dplyr::ungroup()

  # Merge with overlaps count
  result <- n_days_df %>%
    dplyr::left_join(overlaps_count, by = c("Id", "Year"))

  return(result)
}
