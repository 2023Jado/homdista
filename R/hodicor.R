hodicor <- function(cormethod){

    # Correlation
  correlation_dist_homer <- cor.test(as.numeric(final_file$Distance_km), as.numeric(final_file$Area_km2), method = cormethod,
                                     conf.level = 0.95)

  # Create a scatter plot with a fitted line

  # Convert Month and Year columns to factors
  final_file$Month <- factor(final_file$Month)
  final_file$Year <- factor(final_file$Year)

  # Create the scatter plot
  corplot <- qplot(x = Distance_km, y = Area_km2, col= Id, data = final_file) +
    geom_smooth(method = "lm") + xlab("Distance [km]") +
    ylab("Area [km2]") + xlim(min(final_file$Distance_km), max(final_file$Distance_km)) +
    ylim(min(final_file$Area_km2), max(final_file$Area_km2)) + guides(col = guide_legend(title = NULL))

  plot(corplot)

  return(correlation_dist_homer)
}


