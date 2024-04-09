# homdista

The homdista package offers a suite of functions tailored for estimating habitat utilization area and traveled distance.
These functions are:

**homdista::homdista():** Computes the area utilized and distance walked per month and year.

**homdista::homekde():** Generates polygons representing the utilized areas for each group.

**homdista::hodicor():** Computes correlation values and plots the correlation between area and distance.

**homdista::distwalk():** Generates line paths representing the traveled distance.

**homdista::moveObject():** Converts the data frame into a move object for further movement analysis.

These functions collectively provide comprehensive tools for analyzing movement patterns and habitat utilization.

**Parameters:**

**file:** R-imported dataframe which comprises at least three columns: a longitude column labeled "x", a latitude column labeled "y", and a timestamp column labeled "timestamp", in lowercase.

**tf:** timestamp format

**crs_epsg:** the epsg code related to the dataset coordinates

**Id_name:** Column name from dataset which shows different categories (e.g., different groups (group A, group B, group C, ...))

**perc:** The percentage utilized to calculate the KDE home range at a specific level (e.g., 50% for core areas, 75%, 90%, 95%, ...).

**parh:** Bandwidth or smoothing parameter

**adista:** A layer containing the area and distance values generated from the homdista function

**cormethod:** Correlation method between paired samples ("pearson", "kendall", or "spearman") at confidence level of 95%
&nbsp;
```
# Estimating the home range size and walked distances
homdista(file, tf, crs_epsg, Id_name, perc, parh)

# Home range polygons
homekde(file, tf, crs_epsg, Id_name, perc, parh)

# Determining and plotting the correlation between walked distance and utilized home range area
hodicor(adista, cormethod)

# Walked distance line paths
distwalk(file, tf, crs_epsg, Id_name)

# Data frame to move object
moveObject(file, tf, Id_name, crs_epsg)
```
Below is the example code
```
library(homdista)

library(lubridate)
library(sf)
library(sp)
library(adehabitatLT)
library(adehabitatHR)
library(mapview)
library(ggplot2)
library(move)

# Read the file
file <- read.csv("C:/Users/Jado/Documents/EAGLE/Semester2/Animal_movement/Data/data.csv", header=T)

# Estimating the home range size and walked distances per month
Homerange_distance <- homdista::homdista(file ,"%m/%d/%y %I:%M %p", 32734, "Animal", 90, 500)

# Determining and plotting the correlation between walked distance and utilized home range area
Correlation <- homdista::hodicor(Homerange_distance, "spearman")

# Walked distance line paths
Distance <- homdista::distwalk(file, "%m/%d/%y %I:%M %p", 32734, "Animal")
mapview(Distance)

# Home range polygons
Homerange <- homdista::homekde(file, "%m/%d/%y %I:%M %p", 32734, "Animal", 90, 500)
st_as_sf(Homerange)
palette <- rainbow(length(unique(Homerange$Id)))
mapview(Homerange, zcol = "Id", col.regions = palette,
               legend = TRUE, legend.title = "", legend.values = unique(Homerange$Id))

# Data frame to move object
Move <- homdista::moveObject(file, "%m/%d/%y %I:%M %p", "Animal", 32734)
plot(Move)
move::distance(Move)
move::angle(Move)
```
