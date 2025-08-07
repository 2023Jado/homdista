#' Computing home range overlap among different animal individuals, groups, etc.
#' @author Jean de Dieu Tuyizere
#'
#' @param data This refers to the result data from the `homeyear` function.
#' @param epsg_crs This is the EPSG code for the coordinate reference system (CRS) to be used for the analysis.
#'
#' @return A layer whith the overlap information for each individual, group, etc. This can be also visualized in GIS software.
#' @export
#'
#' @examples
#' data <- homekde(file, crs_epsg, Id_name, timestamp, perc, parh)
#' homeoverlap <- homoverlap(data, crs_epsg)
#'
#' @import sf
#' @import dplyr
#' @import purrr
#' @import units

yearoverlap <- function(data, crs_epsg){

  # validating and projecting the data
  habitat_sf <- st_make_valid(data)
  habitat_sf <- st_transform(habitat_sf, crs = crs_epsg)

  # Split by time period
  by_time <- split(habitat_sf, habitat_sf$Year)

  # Function to compute overlap information
  compute_overlap <- function(time_data) {
    result <- list()

    for (i in seq_len(nrow(time_data))) {
      this_group <- time_data[i, ]
      this_id <- this_group$Id
      this_year <- this_group$Year
      this_geom <- st_geometry(this_group)
      this_area <- this_group$area_km2

      overlaps <- list()
      total_overlap_area <- 0

      for (j in seq_len(nrow(time_data))) {
        if (i == j) next
        other_group <- time_data[j, ]
        other_id <- other_group$Id
        other_geom <- st_geometry(other_group)

        # Try intersection
        inter <- tryCatch(st_intersection(this_geom, other_geom), error = function(e) NULL)

        if (!is.null(inter) && length(inter) > 0) {
          area_overlap <- st_area(inter) %>% set_units("km^2") %>% drop_units()
          total_overlap_area <- total_overlap_area + area_overlap

          overlaps[[length(overlaps) + 1]] <- data.frame(
            Id = this_id,
            Year = this_year,
            overlapped_with = other_id,
            overlapped_area_km2 = as.numeric(area_overlap),
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(overlaps) == 0) {
        # No overlaps
        overlaps[[1]] <- data.frame(
          Id = this_id,
          Year = this_year,
          overlapped_with = "no overlap",
          overlapped_area_km2 = 0,
          stringsAsFactors = FALSE
        )
      }

      # Add total and unoverlapped to all rows
      overlaps_df <- bind_rows(overlaps) %>%
        mutate(
          area_km2 = this_area,
          total_overlapped_area_km2 = total_overlap_area,
          unoverlapped_area_km2 = this_area - total_overlap_area
        ) %>%
        bind_cols(geometry = this_geom)

      result[[i]] <- overlaps_df
    }

    st_as_sf(bind_rows(result))
  }

  # Apply per time period
  result_sf <- by_time %>%
    map_dfr(compute_overlap)


  # Returning the result
  return(result_sf)
}
