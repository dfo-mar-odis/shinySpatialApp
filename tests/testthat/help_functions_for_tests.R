# ----- RENDER CHECK DELETE -----
# This function is used to ensure that an rmarkdown file can be run without error.
# It will attempt to render the file, check if an output file was generated, and if so, delete the output file
#
# Inputs:
# 1. Section path: path to the Rmarkdown file.
# 2. delete_file: Optional Boolean, if not set, deletes the output file.
#
# Outputs: 
# 1. Success, indicates whether the file was correctly rendered
#
# Developed by Quentin Stoyel, Summer 2021

render_check_delete <- function(sectionPath, deleteFile=TRUE) {
  load_test_data(environment())
  test_out_dir <- here::here("tests/temp")
  unlink(test_out_dir) # empty the folder
  
  catch_all_output({
    out_html <- rmarkdown::render(sectionPath, output_dir = test_out_dir)
    })
  
  success <- file.exists(out_html)
  if (file.exists(out_html) && deleteFile) {
    file.remove(out_html)
  }
  
  return(success)
}


# ----- Load Test Data -----
# This function is used to load necessary data into the environment for testing.
# Very similar to the initial chunks used in the first r markdown script.
#
# Inputs: None
#
# Outputs: None
#
# Developed by Quentin Stoyel, Summer 2021


load_test_data <- function(testEnv) {
  
  load(here::here("app/data/testData.RData"), envir = testEnv)
  load(here::here("app/data/testData.RData"), envir = environment())
  lapply(list.files(here::here("app/R"), pattern = ".[Rr]$", full.names = TRUE), source, local=testEnv)
  lapply(list.files(here::here("app/R"), pattern = ".[Rr]$", full.names = TRUE), source, local=environment())

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
  out_list <- list("minYear" = 2010,
                   "Region" = Region,
                   "site" = site,
                   "studyBox_geom" = studyBox_geom, 
                   "areaMap" = areaMap, 
                   "bboxMap" = bboxMap,
                   "regionBox" = regionBox, 
                   "regionMap" = regionMap,
                   "studyArea" = studyArea)
  
  
  list2env(out_list, env=testEnv)
  
  }


catch_all_output <- function(codeChunk) {
  suppressWarnings({
    utils::capture.output({
      utils::capture.output({codeChunk}, file = "NUL", type = "output")
      }, file = "NUL", type = "message")
  })
}
