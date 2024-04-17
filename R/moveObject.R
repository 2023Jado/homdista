#' Data frame to Move object
#'
#' Arguments
#' @param file R-imported dataframe which comprises at least three columns: a longitude column labeled "x", a latitude column labeled "y", and a timestamp column labeled "timestamp", in lowercase.
#' @param tf timestamp format
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#' @param crs_epsg timestamp format
#'
#' @return move object
#' @export
#'
#' @examples
#'file_path <- system.file("extdata", "data.csv", package = "homdista")
#' file <- read.csv(file_path, header=T)
#'
#' #Run the following libraries
#' library(sp)
#' library(sf)
#' library(lubridate)
#' library(mapview)
#' library(move)
#' library(webshot)
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
#' #Make the move object from data frame
#' Move <- moveObject(file, tf, Id_name, crs_epsg)
#' plot(Move)
#' mapview(Move)
#'
#'
moveObject <- function(file, tf, Id_name, crs_epsg){
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

  # Change the projection
  crscode <- crs_epsg
  crs(df_move) <- CRS(paste0("+init=epsg:", crs_epsg))
  return(df_move)

}

