
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ClusterApp

<!-- badges: start -->
<!-- badges: end -->

The goal of ClusterApp is a Shiny application to guide and streamline
cluster studies based on GPS data.

## Installation

You can install the development version of ClusterApp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JohannaMz/ClusterApp")
```

The app can then be started by the command:

``` r
library(ClusterApp)
#> The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
#> which was just loaded, will retire in October 2023.
#> Please refer to R-spatial evolution reports for details, especially
#> https://r-spatial.org/r/2023/05/15/evolution4.html.
#> It may be desirable to make the sf package available;
#> package maintainers should consider adding sf to Suggests:.
#> The sp package is now running under evolution status 2
#>      (status 2 uses the sf package in place of rgdal)
ClusterApp::run_app()
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>

## Getting help

If you encounter a bug, please [submit an
issue](https://github.com/JohannaMz/ClusterApp/issues). For more general
questions and suggestions, contact [Johanna Märtz](johanna@maertz.eu).

## Citation

``` r
citation("ClusterApp")
```
