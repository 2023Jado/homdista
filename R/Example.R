# Read the file

file <- read.csv("C:/Users/Jado/Documents/EAGLE/Semester 2/Data/African elephant Jammes Hwange NP2.csv", header=T)

# Define some parameters

tf <- "%m/%d/%y %H:%M"
Id_name <- "Animal"
crs_epsg <- 32734
perc <- 95

library(homdista)

# Compute the area utilized and distance traveled by elephant with "homdista"
area_distance <- homdista::homdista(file, tf, crs_epsg, Id_name, perc)

# Home range spatial polygons "homkde"
homerange_polygons <- homdista::homekde(file, tf, crs_epsg, Id_name, perc)

# Check the correlation between area used and traveled distance
corr_home_distance <- homdista::hodicor(area_distance, "spearman")

# Spatial lines (paths) showing traveled distance
distance_paths <- homdista::distwalk(file, tf, crs_epsg, Id_name)
mapview(distance_paths)
