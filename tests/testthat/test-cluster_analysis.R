 library(testthat)
# library(dplyr)
# library(sf)
# library(lubridate)
# library(sftrack)
# library(tidyr)
# library(readr)
# library(openxlsx)
# library(readxl)


test_that("Cluster analysis function works as expected", {

  skip_if_not(interactive())

  intensive.start = as.Date("2023-08-01")
  intensive.end = as.Date("2024-08-02")
   datapoints =
  data.frame(
    Collar_ID = c("A", "A", "A", "A"),
    LMT_date = as.POSIXct(c("2023-08-01 10:00:00", "2023-08-01 11:00:00",
                 "2023-08-01 12:00:00", "2023-08-01 13:00:00")),
    Latitude = c(15.63312, 15.6402, 15.64022, 15.64026), #c(490120.246686, 491130.389923, 491500.412843, 491700.413110),
    Longitude = c(62.08441, 62.08587, 62.08587, 62.08583) #62.08441, 62.08587, 62.08587, 62.08583) #2899890.312071, 2899889.999744, 2899890.888014, 2899891.287740
  )
  sep = ","
  ID = "Collar_ID"
  LMT_Date = "LMT_date"
  East = "Latitude"
  North = "Longitude"
  dateFormat = "%Y-%m-%d %H:%M:%S"
  prepostPeriod = 0
  EPSGcode = 4326 #32617
  buffer = 100
  count = 2
  indID = "label"
  lastClustersFile = "No latest cluster file."
  minute_diff = NA
  oldclusters = FALSE
  onlyClusters = FALSE
  UTM_zone =  "33N" #"17N"

  # Test if the function runs without errors
  expect_no_error(cluster_analysis(intensive.start, intensive.end, datapoints,sep, ID, LMT_Date, East, North,
                                   dateFormat, prepostPeriod, EPSGcode, buffer, count,indID, lastClustersFile,
                                   minute_diff, oldclusters,onlyClusters, UTM_zone))

  # Test if the output is a list
  expect_type(cluster_analysis(intensive.start, intensive.end, datapoints,sep, ID, LMT_Date, East, North,
                             dateFormat, prepostPeriod, EPSGcode, buffer, count,indID, lastClustersFile,
                             minute_diff, oldclusters, onlyClusters, UTM_zone), "list")

  # Test if the output list contains the expected elements
  expect_named(cluster_analysis(intensive.start, intensive.end, datapoints,sep, ID, LMT_Date, East, North,
                                dateFormat, prepostPeriod, EPSGcode, buffer, count,indID, lastClustersFile,
                                minute_diff, oldclusters,onlyClusters, UTM_zone), c("Clusters_sf", "Join_sf", "data_sf_traj", "status", "settings"))


})
