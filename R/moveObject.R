#' Data frame to Move object
#'
#' @param file file data frame already read in R and has at least the following three columns:
#' longitude column named as "x", latitude column named as "y", and timestamp named as "timestamp" (both in lower case).
#' @param tf timestamp format
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#' @param crs_epsg timestamp format
#'
#' @return move object
#' @export
#'
#' @examples
#'
#' file <- read.csv("C:/Users/Jado/Documents/EAGLE/Semester 2/Data/African elephant Jammes Hwange NP2.csv", header=T)
#'
#'  # Define some parameters
#'tf <- "%m/%d/%y %H:%M"
#'Id_name <- "Animal"
#'crs_epsg <- 32734
#'perc <- 95
#'
#' library(homdista)
#'
#' #Make the move object from data frame
#' moveObj <- homdista::moveObject(file, tf, Id_name, crs_epsg)
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
  duplicate_indices <- duplicated(no_na_df_sorted$time) | #If I don't do this, why do I get the error that dataset includes double timestamps?
    duplicated(no_na_df_sorted$time, fromLast = TRUE)     #I loose many rows if I apply this (however, it is the only option I have so far)

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
}