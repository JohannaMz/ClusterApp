
library(shinytest2)

#testthat::skip_on_ci()


test_that("{shinytest2} recording: wolf_demo_workflow", {

  testthat::skip_on_cran()

  app <- AppDriver$new(name = "wolf_demo_workflow", height = 603, width = 979)
  app$set_inputs(demo_data = "Demo data wolf")
  app$set_inputs(LMT_Date = "LMT_date")
  app$set_inputs(East = "Latitude")
  app$set_inputs(North = "Longitude")
  app$set_inputs(UTM_zone = 33)
  app$set_inputs(indID = "wolf_demo")
  app$set_inputs(buffer = 50)
  app$set_inputs(count = 2)
  app$set_inputs(intensivePeriod = c("2022-02-15", "2023-10-28"))
  app$set_inputs(intensivePeriod = c("2022-02-15", "2022-03-17"))
  app$click("doit")
  app$expect_values()
})


test_that("{shinytest2} recording: bears_demo_workflow", {

  testthat::skip_on_cran()

  app <- AppDriver$new(name = "bears_demo_workflow", height = 603, width = 979)
  app$set_inputs(demo_data = "Demo data bears")
  app$set_inputs(LMT_Date = "LMT_date")
  app$set_inputs(East = "Latitude")
  app$set_inputs(North = "Longitude")
  app$set_inputs(UTM_zone = 33)
  app$set_inputs(indID = " u")
  app$set_inputs(indID = "multiple")
  app$set_inputs(indID = "multiplebears_demo")
  app$set_inputs(buffer = 100)
  app$set_inputs(count = 2)
  app$set_inputs(intensivePeriod = c("2014-05-01", "2023-10-29"))
  app$set_inputs(intensivePeriod = c("2014-05-01", "2014-05-31"))
  app$set_inputs(minute_diff = 60)
  app$set_inputs(onlyClusters = TRUE)
  app$click("doit")
  app$expect_values()
})
