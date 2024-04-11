
<!-- README.md is generated from README.Rmd. Please edit that file -->

# homdista

<!-- badges: start -->
<!-- badges: end -->

The purpose of homdista is to analyze the movements of objects, such as
animals, based on collected GPS coordinates. This package provides
several functions to calculate home range areas and distances traveled
over months and years.

    homdista::homdista(): Computes monthly and yearly utilized areas and distances walked.

    homdista::homekde(): Generates polygons representing the utilized areas.

    homdista::hodicor(): Calculates correlation values and visualizes the relationship between area and distance.

    homdista::distwalk(): Creates line paths to visualize the traveled distances.

    homdista::moveObject(): Converts a data frame into a move object for more detailed movement analysis.

## Installation

You can install the homdista package from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("2023Jado/homdista")
```

## To effectively use homdista for data analysis, you will need to install and use the following additional packages

    library(ggplot2)
    library(mapview)
    library(tidyr)
    library(move)

## Example

The following are basic examples for how each of the functions works:

``` r
options(warn = -1)
library(homdista)
## Additional packages
library(sp)
library(sf)
#> Linking to GEOS 3.11.2, GDAL 3.7.2, PROJ 9.3.0; sf_use_s2() is TRUE
library(ade4)
library(adehabitatMA)
#> Registered S3 methods overwritten by 'adehabitatMA':
#>   method                       from
#>   print.SpatialPixelsDataFrame sp  
#>   print.SpatialPixels          sp
library(CircStats)
#> Loading required package: MASS
#> Loading required package: boot
library(adehabitatLT)
library(adehabitatHR)
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
library(tidyr)

## Home range area (with Kernel density estimator) and walked distance calculation
file <- read.csv("data.csv", header = T)
area_distance <- homdista(file ,"%m/%d/%y %I:%M %p", 32734, "Animal", 90, 500)
#> Deleting KDE result for 7 2009 elephant due to fewer than 5 relocations.
#> Deleting subset for 7 2009 elephant due to fewer than 5 relocations.

head(area_distance)
#>   Month Year       Id      Distance_km  Area_km2
#> 1     1 2010 elephant 4214388.24853781  14.70935
#> 2     1 2013 elephant 11125730.8726069 415.80637
#> 3     1 2014 elephant 11881704.5000014 189.73659
#> 4     1 2016 elephant 2775637.76426774  95.63485
#> 5     1 2017 elephant 2163398.18108898 420.97294
#> 6    10 2010 elephant 1740522.35195486 865.69368
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
