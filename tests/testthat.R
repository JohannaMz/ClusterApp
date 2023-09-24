Sys.setenv("R_TESTS" = "")

library(testthat)
library(ClusterApp)

test_check("ClusterApp")

