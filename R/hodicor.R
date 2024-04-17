#' Correlation between the area utilized and traveled distance
#'
#' Arguments
#' @param adista a layer containing the area and distances values generated from the homdista function
#' @param cormethod correlation method between paired samples (pearson", "kendall", or "spearman")
#'
#' @return correlation values
#' @export
#'
#' @examples
#' file_path <- system.file("extdata", "data.csv", package = "homdista")
#'  file <- read.csv(file_path, header=T)
#'
#' #Run the following libraries
#' library(sp)
#' library(sf)
#' library(ade4)
#' library(adehabitatMA)
#' library(CircStats)
#' library(adehabitatLT)
#' library(adehabitatHR)
#' library(lubridate)
#' library(ggplot2)
#' library(tidyr)
#'
#'
#' # Define some parameters
#' tf <- "%m/%d/%y %I:%M %p"
#' Id_name <- "Animal"
#' crs_epsg <- 32734
#' perc <- 95
#' parh <- 500
#'
#' library(homdista)
#'
#' # Compute the area utilized and distance traveled by elephant
#' area_distance <- homdista(file, tf, crs_epsg, Id_name, perc, parh)
#'
#' # Check the correlation between area used and traveled distance using "spearman method"
#' corr_home_distance <- hodicor(area_distance, "spearman")
#' corr_home_distance
#'
#'
hodicor <- function(adista, cormethod){

  # Re-read the file name returned from homdista

  final_file <- adista

  # Change the columns of area and distance as numeric
  final_file$Distance_km <- as.numeric(final_file$Distance_km)
  final_file$Area_km2 <- as.numeric(final_file$Area_km2)

    # Correlation
  correlation_dist_homer <- cor.test(as.numeric(final_file$Distance_km), as.numeric(final_file$Area_km2), method = cormethod,
                                     conf.level = 0.95)

  # Create a scatter plot with a fitted line

  # Convert Month and Year columns to factors
  final_file$Month <- factor(final_file$Month)
  final_file$Year <- factor(final_file$Year)

  # Create the scatter plot
  corplot <- qplot(x = Distance_km, y = Area_km2, data = final_file) +
    geom_smooth(method = "lm") + xlab("Distance [km]") +
    ylab("Area [km2]") + xlim(min(final_file$Distance_km), max(final_file$Distance_km)) +
    ylim(min(final_file$Area_km2), max(final_file$Area_km2))

  plot(corplot)

  return(correlation_dist_homer)
}


