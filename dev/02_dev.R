# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering
attachment::att_amend_desc()



## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
usethis::use_package("shiny")
usethis::use_package("shinyalert")
usethis::use_package("dplyr")
usethis::use_package("DT")
usethis::use_package("foreign")
usethis::use_package("leaflet")
usethis::use_package("readr")
usethis::use_package("sf")
usethis::use_package("shinyFiles")
usethis::use_package("shinyWidgets")
usethis::use_package("tmap")
usethis::use_package("utils")
usethis::use_package("lubridate")
usethis::use_package("raster")
usethis::use_package("hms")
usethis::use_package("sftrack")
usethis::use_package("stats")
usethis::use_package("tidyr")
usethis::use_package("readxl")
usethis::use_package("openxlsx")
usethis::use_package("golem")
usethis::use_package("shinytest2")
usethis::use_package("methods")
usethis::use_package("stringr")
usethis::use_package("htmlwidgets")
#usethis::use_package("rlang")
usethis::use_pipe()





## Add modules ----
## Create a module infrastructure in R/
#golem::add_module(name = "name_of_module1", with_test = TRUE) # Name of the module
#golem::add_module(name = "name_of_module2", with_test = TRUE) # Name of the module


## Add helper functions ----
## Creates fct_* and utils_*
# golem::add_fct("helpers", with_test = TRUE)
# golem::add_utils("helpers", with_test = FALSE)


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
# golem::add_css_file("custom")
# golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "bears", open = FALSE)

bears <- read_delim("C:/Users/johan/Documents/ClusterApp Data/bear/bears.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)

usethis::use_data(bears, overwrite = TRUE)
usethis::use_r(name = "bears", open = T)


##add wolf data
usethis::use_data_raw(name = "wolf", open = FALSE)

wolf <- read_delim("C:/Users/johan/Documents/ClusterApp Data/Wolf/wolf.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)

usethis::use_data(wolf, overwrite = TRUE)
usethis::use_r(name = "wolf", open = T)


## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("ClusterApp")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
#usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
#covrpage::covrpage()

## CI ---- Continuous integration
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()

# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
#usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
#usethis::use_github_action_check_full()
pkgcheck::use_github_action_pkgcheck()

#codemeta.json file
codemetar::write_codemeta()

# Add action for PR (pull Request)
#usethis::use_github_action_pr_commands()

# # Travis CI
# usethis::use_travis()
# usethis::use_travis_badge()
#
# # AppVeyor
# usethis::use_appveyor()
# usethis::use_appveyor_badge()
#
# # Circle CI
# usethis::use_circleci()
# usethis::use_circleci_badge()
#
# # Jenkins
# usethis::use_jenkins()
#
# # GitLab CI
# usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
