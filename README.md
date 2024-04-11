
<!-- README.md is generated from README.Rmd. Please edit that file -->

# homdista

<!-- badges: start -->
<!-- badges: end -->

The purpose of homdista is to analyze the movements of objects, such as
animals, based on collected GPS coordinates. This package provides
several functions to calculate home range areas and distances traveled
over months and years.

homdista::homdista(): Computes monthly and yearly utilized areas and
distances walked.

homdista::homekde(): Generates polygons representing the utilized areas
for different groups.

homdista::hodicor(): Calculates correlation values and visualizes the
relationship between area and distance.

homdista::distwalk(): Creates line paths to visualize the traveled
distances.

homdista::moveObject(): Converts a data frame into a move object for
more detailed movement analysis.

## Installation

You can install the development version of homdista from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("2023Jado/homdista")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(homdista)
## basic example code
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
