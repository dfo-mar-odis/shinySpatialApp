library(testthat)


# ---------------------PREPARE DATA--------------------------
source(here::here("app/tests/helper_functions_for_tests.R"))

# prepare test environment, similar to data loading in into_EN.rmd
load_test_data()

# select random sample area from test sample areas
studyAreaOpts <- here::here("app/studyAreaTest/", c("geoms_slc_coastal_test.geojson", 
                                              "geoms_slc_no_land.geojson", 
                                              "geoms_slc_coastal_test.geojson"))



studyArea <- st_read(sample(studyAreaOpts, 1))

# prepped_data is a list of needed variables, eg site, studyBox_geom, etc.
preppedData <- prep_test_data(studyArea)
list2env(preppedData, env=globalenv())


test_that("default plot", {
  testPlot <- default_ggplot()
  expect_equal(length(testPlot$data), 0)
  expect_equal(testPlot$labels$x, "Longitude")
  expect_equal(testPlot$labels$y, "Latitude")
})

test_that("region plot", {
  testPlot <- region_map(regionBox, studyArea, land10m_sf, bounds_sf)
  expect_equal(length(testPlot$data), 0)
  expect_equal(length(testPlot$layers), 5)
  expect_equal(testPlot$labels$x, "Longitude")
  expect_equal(testPlot$labels$y, "Latitude")
})

test_that("area plot list", {
  testPlotList <- area_map(studyArea, site, land50k_sf, 5, bounds_sf, studyBox_geom)
  expect_equal(length(testPlotList[[1]]$data), 0)
  expect_equal(length(testPlotList[[1]]$layers), 6)
  expect_equal(class(testPlotList[[2]]), "bbox")
})
