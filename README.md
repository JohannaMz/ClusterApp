
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ClusterApp <img style="padding: 15px 0px 0px 0px;"  src='inst/app/www/favicon_withtext.png' align="right" height="140"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/JohannaMz/ClusterApp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JohannaMz/ClusterApp/actions/workflows/R-CMD-check.yaml)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- ![pkgcheck](https://github.com/JohannaMz/ClusterApp/workflows/pkgcheck/badge.svg)](https://github.com/JohannaMz/ClusterApp/actions?query=workflow%3Apkgcheck) -->
<!-- badges: end -->

The ClusterApp is a Shiny application to guide and streamline cluster
studies based on GPS data.

## Installation

For the installation to work, you need
[Rtools](https://cran.r-project.org/bin/windows/Rtools/) downloaded. You
can install the development version of ClusterApp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JohannaMz/ClusterApp", build_vignettes = TRUE)
```

For re-installations of new versions make sure to remove old versions
and restart the session:

``` r
remove.package("ClusterApp")#if the package is loaded before
.rs.restartR() #restart R session to avoid any problms

devtools::install_github("JohannaMz/ClusterApp", build_vignettes = TRUE)
```

The app can then be started by the command:

``` r
library(ClusterApp)
ClusterApp::run_app()
```

When the app started, it can take a few seconds unti it recognizes the
folder structure.

## Tutorial

A tutorial for the usage of the app can be found by accessing the
vignette (html file):

``` r
browseVignettes("ClusterApp")
```

## Disclaimer

This is an early release and functionalities might change. This package
is currently pending a peer-review and detailed description will be
available after it is published. I take no responsibility for the proper
functioning of this package. If you have any questions, concerns or you
would simply like to apply it to your data, I encourage to contact me
directly.

## Getting help

If you encounter a bug, please [submit an
issue](https://github.com/JohannaMz/ClusterApp/issues). For more general
questions and suggestions, contact Johanna Märtz (<johanna@maertz.eu>).

## Citation

``` r
citation("ClusterApp")
#> Warning in citation("ClusterApp"): could not determine year for 'ClusterApp'
#> from package DESCRIPTION file
#> To cite package 'ClusterApp' in publications use:
#> 
#>   Märtz J, Tallian A, Wikenros C, Heeres R (????). _ClusterApp: A Shiny
#>   R application to guide and streamline cluster studies based on GPS
#>   data_. R package version 0.0.0.9000,
#>   <https://github.com/JohannaMz/ClusterApp>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ClusterApp: A Shiny R application to guide and streamline cluster studies based
#> on GPS data},
#>     author = {Johanna Märtz and Aimee Tallian and Camilla Wikenros and Rick Heeres},
#>     note = {R package version 0.0.0.9000},
#>     url = {https://github.com/JohannaMz/ClusterApp},
#>   }
```
