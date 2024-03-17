homekde <- function(homeshape){
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




