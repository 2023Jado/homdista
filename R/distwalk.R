distwalk <- function(){

  # Merge the computed distances with the spatial data based on the 'Code' column
  df_move_with_distances <- merge(df_move_sorted, traveled_distances_df, by = "Code")

  # Initialize an empty list to store paths and associated code name
  lines_list <- list()
  codes <- character(0)

  # Loop through each code name
  for (code in unique(df_move_with_distances$Code)) {

    group_df <- df_move_with_distances[df_move_with_distances$Code == code, ]
    if (nrow(group_df) > 1) {
      line <- st_cast(st_union(st_cast(group_df, "MULTIPOINT")), "LINESTRING")
      lines_list[[code]] <- line
      codes <- c(codes, code)  # Add code to codes vector
    } else {
      lines_list[[code]] <- NA  # Indicate missing lines
    }
  }

  # Create a data frame with codes and corresponding lines
  lines_df <- data.frame(Code = rep(codes, sapply(lines_list, length)),
                         geometry = do.call("c", lines_list),
                         row.names = NULL)

  # Convert to sf object
  Movement <- st_as_sf(lines_df)

  mapview(Movement)

  return(Movement)
}
