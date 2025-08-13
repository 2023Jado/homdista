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
#' homeoverlap <- yearoverlap(data, 32734)
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

  compute_overlap <- function(time_data) {
    result <- list()

    for (i in seq_len(nrow(time_data))) {
      this_group <- time_data[i, ]
      this_id <- this_group$Id
      this_year <- this_group$Year
      this_area <- this_group$area_km2
      this_geom <- st_geometry(this_group)

      overlaps_info <- list()
      overlap_geoms <- list()

      # find overlaps with all others
      for (j in seq_len(nrow(time_data))) {
        if (i == j) next
        other_group <- time_data[j, ]
        other_id <- other_group$Id
        other_geom <- st_geometry(other_group)

        inter <- tryCatch(st_intersection(this_geom, other_geom), error = function(e) NULL)

        if (!is.null(inter) && length(inter) > 0 && !all(st_is_empty(inter))) {
          area_overlap <- st_area(inter) %>% set_units("km^2") %>% drop_units()

          overlaps_info[[length(overlaps_info) + 1]] <- data.frame(
            Id = this_id,
            Year = this_year,
            overlapped_with = other_id,
            overlapped_area_km2 = as.numeric(area_overlap),
            stringsAsFactors = FALSE
          )

          overlap_geoms[[length(overlap_geoms) + 1]] <- inter
        }
      }

      if (length(overlap_geoms) > 0) {
        # union all overlap geometries
        overlap_union <- st_union(do.call(c, overlap_geoms))
        total_overlap_area <- st_area(overlap_union) %>% set_units("km^2") %>% drop_units()

        # overlapped part
        overlaps_df <- bind_rows(overlaps_info) %>%
          mutate(
            area_km2 = this_area,
            total_overlapped_area_km2 = total_overlap_area,
            unoverlapped_area_km2 = this_area - total_overlap_area
          )
        overlaps_sf <- st_sf(overlaps_df, geometry = st_sfc(overlap_union))

        # unoverlapped part
        unoverlapped_geom <- tryCatch(st_difference(this_geom, overlap_union), error = function(e) NULL)

        if (!is.null(unoverlapped_geom) &&
            length(unoverlapped_geom) > 0 &&
            !all(st_is_empty(unoverlapped_geom))) {

          unoverlap_df <- data.frame(
            Id = this_id,
            Year = this_year,
            overlapped_with = "unoverlapped",
            overlapped_area_km2 = 0,
            area_km2 = this_area,
            total_overlapped_area_km2 = total_overlap_area,
            unoverlapped_area_km2 = as.numeric(st_area(unoverlapped_geom) %>% set_units("km^2") %>% drop_units()),
            stringsAsFactors = FALSE
          )
          unoverlap_sf <- st_sf(unoverlap_df, geometry = st_sfc(unoverlapped_geom))

          overlaps_sf <- rbind(overlaps_sf, unoverlap_sf)
        }

      } else {
        # no overlaps — whole geometry unoverlapped
        no_overlap_df <- data.frame(
          Id = this_id,
          Year = this_year,
          overlapped_with = "unoverlapped",
          overlapped_area_km2 = 0,
          area_km2 = this_area,
          total_overlapped_area_km2 = 0,
          unoverlapped_area_km2 = this_area,
          stringsAsFactors = FALSE
        )
        overlaps_sf <- st_sf(no_overlap_df, geometry = this_geom)
      }

      result[[i]] <- overlaps_sf
    }

    do.call(rbind, result)
  }

  # Apply per time period
  result_sf <- by_time %>%
    map_dfr(compute_overlap)

  # Returning the result
  return(result_sf)
}
