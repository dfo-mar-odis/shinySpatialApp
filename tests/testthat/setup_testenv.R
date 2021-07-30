
# ---------------------PREPARE DATA--------------------------
source(here::here("tests/testthat/help_functions_for_tests.R"))
message("Generate new test data")
catch_all_output({
  # generate a new testData file
  source(here::here("dataprocessing/MakeTestData.R"))
  gen_all_test_data()
  
  
  load(here::here("app/data/testData.RData"))
  lapply(list.files(here::here("app/R"), pattern = ".[Rr]$", full.names = TRUE), source)
  
  # select random sample area from test sample areas
  studyAreaOpts <- here("app/studyAreaTest/", c("geoms_slc_coastal_test.geojson", 
                                                "geoms_slc_no_land.geojson", 
                                                "geoms_slc_coastal_test.geojson"))
  
  studyArea <- st_read(sample(studyAreaOpts, 1))
  
  site <- sf::st_centroid(studyArea)
  
  studyBox_geom <- geom_sf(data=studyArea, fill=NA, col="red", size=1) 
  
  areaMapList <- area_map(studyArea, site, land50k_sf, 5, bounds_sf, studyBox_geom)
  areaMap <- areaMapList[[1]]
  bboxMap <- areaMapList[[2]] 
  Region <- st_read(here::here("app/studyAreaTest/geoms_slc_MarBioRegion.geojson"))
  regionBox <- sf::st_bbox(Region) 
  regionMap <- region_map(regionBox, studyArea, land10m_sf, bounds_sf)  
})

message("Test data generation complete")