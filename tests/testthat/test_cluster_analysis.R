library(testthat)
library(dplyr)
library(sf)
library(lubridate)
library(sftrack)
library(tidyr)

# Load your cluster_analysis function here
source("R/cluster_analysis.R")

# Define some example data for testing
example_data <- data.frame(
  ID = c("A", "A", "A", "A"),
  LMT_Date = c("2023-08-01 10:00:00", "2023-08-01 11:00:00", "2023-08-01 12:00:00", "2023-08-01 13:00:00"),
  East = c(15.63312, 15.6402, 15.64022, 15.64026),
  North = c(62.08441, 62.08587, 62.08587, 62.08583)
)



test_that("Cluster analysis function works as expected", {

  intensive.start = as.Date("2023-08-01")
  intensive.end = as.Date("2023-08-02")
  datapoints = example_data
  ID = "ID"
  LMT_Date = "LMT_Date"
  East = "East"
  North = "North"
  dateFormat = "%Y-%m-%d %H:%M:%S"
  prepostPeriod = 0
  EPSGcode = 4326
  buffer = 100
  count = 2
  indID = "label"
  lastClustersFile = "No latest cluster file."
  minute_diff = NA
  oldclusters = FALSE
  UTM_zone = 33

  # Test if the function runs without errors
  expect_no_error(cluster_analysis(intensive.start, intensive.end, datapoints, ID, LMT_Date, East, North,
                                   dateFormat, prepostPeriod, EPSGcode, buffer, count,indID, lastClustersFile,
                                   minute_diff, oldclusters, UTM_zone))

  # Test if the output is a list
  expect_type(cluster_analysis(intensive.start, intensive.end, example_data, ID, LMT_Date, East, North,
                             dateFormat, prepostPeriod, EPSGcode, buffer, count, lastClustersFile,
                             minute_diff, oldclusters, UTM_zone), "list")

  # Test if the output list contains the expected elements
  expect_named(cluster_analysis(intensive.start, intensive.end, example_data, ID, LMT_Date, East, North,
                                dateFormat, prepostPeriod, EPSGcode, buffer, count, lastClustersFile,
                                minute_diff, oldclusters, UTM_zone), c("Clusters_sf", "Join_sf", "data_sf_traj", "status"))

  # You can add more specific tests for the behavior and output of your function here
  # For example, you can test if the output clusters have the correct structure,
  # if the data transformation steps are working correctly, etc.

})
