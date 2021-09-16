library(testthat)
library(here)
context("Function Tests")


test_that("default plot", {
  list2env(load_test_data(), envir = environment())
  emptyPlot <- ggplot2::ggplot()
  testPlot <- format_ggplot(emptyPlot, regionBox)
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

test_that("master intersect", {
  list2env(load_test_data(), envir = environment())
  # test with points
  testBbox <- sf::st_bbox(whitehead_sf)
  mapDataList$studyArea <- testBbox
  mapDataList$bboxMap <- testBbox
  outputList <- master_intersect(whitehead_sf, mapDataList, getRegion = TRUE)
  expect_equal(nrow(outputList$regionData), nrow(whitehead_sf))
  
  # test with polygons
  testBbox <- sf::st_bbox(ClippedCritHab_sf)
  mapDataList$studyArea <- testBbox
  mapDataList$bboxMap <- testBbox
  outputList <- master_intersect(ClippedCritHab_sf, mapDataList, getRegion = TRUE)
  expect_equal(nrow(outputList$regionData), nrow(ClippedCritHab_sf))
  
  # test null intersect (no data in the Gulf of Guinea):
  testBbox["xmin"] <- 0
  testBbox["xmax"] <- 1
  testBbox["ymin"] <- 0
  testBbox["ymax"] <- 1
  mapDataList$region <- testBbox
  mapDataList$bboxMap <- testBbox
  mapDataList$studyArea <- testBbox
  outputList <- master_intersect(whitehead_sf, mapDataList, getRegion = TRUE)
  expect_null(c(outputList$regionData, outputList$studyData, outputList$mapData, outputList$mapPoints))
})




