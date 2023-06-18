
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!--when changes where made run devtools::build_readme() in 01_start.R -->

# ClusterApp

<!-- badges: start -->
<!-- badges: end -->

The `ClusterApp` is a Shiny App developed for the easy and
straightforward analysis of GPS collared individuals to identify
clusters of visits.

Our aim is to provide a streamlined method to apply cluster analyses to
GPS data without the need to have prior knowledge regarding the use of
geographical or statistical software. To facilitate this, we created the
“ClusterAnalysis” Shiny R application (version 1.0). We expect the app
to contribute to the continuity of methods regarding data collection
(decrease observer biases), straightforward usage for field personnel,
and reliable data management.

The app was built using the `golem` framework.

## Installation

You can install the development version of ClusterApp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JohannaMz/ClusterApp")
```

## Run the app

To launch the `ClusterApp`, load the library and type the following code
into the R console.

``` r
library(ClusterApp)
ClusterApp::run_app()
#> PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.
```

<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>

## How to use the app

maybe there will be a guided tutorial with the bear and wolf data. check
out the preprint [here](url)

## Help

If you encounter a bug, please submit an issue on github? For general
questions and feedback, contact [Johanna Märtz](johanna@maertz.eu).

\##Citation

``` r
citation("ClusterApp")
#> Warning in citation("ClusterApp"): kein Datumsfeld in der Datei DESCRIPTION des
#> Paketes 'ClusterApp'
#> Warning in citation("ClusterApp"): konnte das Jahr für 'ClusterApp' aus der
#> DESCRIPTION des Paketes nicht bestimmen
#> 
#> Um Paket 'ClusterApp' in Publikationen zu zitieren, nutzen Sie bitte:
#> 
#>   Märtz J (????). _ClusterApp: Cluster Analysis for analyzing GPS
#>   data_. R package version 0.0.0.9000.
#> 
#> Ein BibTeX-Eintrag für LaTeX-Benutzer ist
#> 
#>   @Manual{,
#>     title = {ClusterApp: Cluster Analysis for analyzing GPS data},
#>     author = {Johanna Märtz},
#>     note = {R package version 0.0.0.9000},
#>   }
```
