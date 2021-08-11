library(testthat)
context("Function Tests")


test_that("default plot", {
  list2env(load_test_data(), envir = environment())
  emptyPlot <- ggplot2::ggplot()
  testPlot <- format_ggplot(emptyPlot)
  expect_equal(length(testPlot$data), 0)
})

test_that("region plot", {
  list2env(load_test_data(), envir = environment())
  testPlot <- region_map(regionBox, studyArea, land10m_sf, bounds_sf)
  expect_equal(length(testPlot$data), 0)
  expect_equal(length(testPlot$layers), 5)
})

test_that("area plot list", {
  list2env(load_test_data(), envir = environment())
  testPlotList <- area_map(studyArea, site, land50k_sf, 5, bounds_sf, studyBox_geom)
  expect_equal(length(testPlotList[[1]]$data), 0)
  expect_equal(length(testPlotList[[1]]$layers), 6)
  expect_equal(class(testPlotList[[2]]), "bbox")
})




