# homdista

This is the documentation of homdista package. Let us go through an example together.

&nbsp;

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
