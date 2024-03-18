#' Home range spatial polygons
#'
#' @param file data frame file already read in R and has at least three columns named as follows longitude column named as "x", latitude column named as "y", and timestamp named as "timestamp"
#' @param tf timestamp format
#' @param crs_epsg the epsg code related to the dataset coordinates
#' @param Id_name Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))
#' @param perc Percentage which is used to compute the home range utilization i.e kernel density estimation at a given level (percentage) (50% for core areas, 75%, 90%, 95%, ...)
#'
#' @return homeshape
#' @export
#'
#' @examples homekde
homekde <- function(file, tf, crs_epsg, Id_name, perc){
  data_df <- file

  # Rename the column
  names(data_df)[which(names(data_df) == "Id_name")] <- "groupid"

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

  # Assign the projection to the move object
  epsg_code <- crs_epsg
  crs <- CRS(paste0("+init=epsg:", epsg_code))
  proj4string(df_move) <- crs

  ############################ Calculations of home range ##################################################

  # Calculate the bandwidth parameter from the move object using "amt package"
  df_move_track <- amt::make_track(df_move, x, y, time, crs=crs_epsg)
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
      getverticeshr(kde, percent = perc, unout = "km2")
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

  homeshape <- SpatialPolygonsDataFrame(home, home3)

  # ###################### Plot a home range map ####################################

  # Create mapview for each homerange separately

  homeranges <- lapply(unique(home$Code), function(code_home) {
    mapview(home[home$Code == code_home, ],
            col.regions = "transparent",
            col = rainbow(length(unique(home$Code))),
            layer.name = code_home,
            alpha.regions = 0,
            legend.opacity = 1,
            alpha = 1,
            lwd = 2
    )
  })

  print(homeranges)
  return(homeshape)
}




