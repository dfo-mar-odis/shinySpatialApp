library(testthat)
# test plot polygons function
minYear <- 2010
load("../data/OpenData.RData")
lapply(list.files("../R", pattern = ".[Rr]$", full.names = TRUE),source)
studyArea <- st_read("../studyAreaTest/geoms_slc_coastal_test.geojson")#coastal 
Region <- st_read("../studyAreaTest/geoms_slc_MarBioRegion.geojson")
st_agr(studyArea) = "constant"
st_agr(bounds_sf) = "constant"
st_agr(land10m_sf) = "constant"
st_agr(land50k_sf) = "constant"
site <- sf::st_centroid(studyArea)

# The following defines studyBox geometry "look". studyBox_geom is input into area map or can be added to any map later
studyBox_geom <- geom_sf(data=studyArea, fill=NA, col="red", size=1) 

# The following plots area map using function (output is a list)
areaMapList <- area_map(studyArea, site, land50k_sf, 5, bounds_sf, studyBox_geom)

# The following separates items in the output list: first item is a map and second is a bounding box of the map
areaMap=areaMapList[[1]] # map
bboxMap=areaMapList[[2]] #bounding box of the map

# Bounding box for the region
regionBox <- sf::st_bbox(Region) 

# Create the regional map
regionMap <- region_map(regionBox, studyArea, land10m_sf, bounds_sf)

test_that("check plot_polygons", {
  
  poly_sf=rockweed_sf
  attribute="RWP"

  clipped <- poly_intersect(poly_sf,Region,studyArea, bboxMap)
  out_plot <- plot_polygons(areaMap,bboxMap,studyBox_geom, clipped$mapPoly,attribute)
  expect_equal(out_plot$labels$x, "Longitude")
  expect_equal(out_plot$labels$y, "Latitude")
  expect_equal(out_plot$labels$fill, "Rockweed")
})




test_that("check poly_intersect", {
  
  poly_sf=rockweed_sf
  attribute="RWP"

  clipped_list  <- poly_intersect(poly_sf,Region,studyArea, bboxMap)
  expect_equal(length(clipped_list), 3)
  expect_gte(sum(st_area(clipped_list$mapPoly)), sum(st_area(clipped_list$studyPoly)))
  expect_gte(sum(st_area(clipped_list$regionPoly)), sum(st_area(clipped_list$mapPoly)))

})
