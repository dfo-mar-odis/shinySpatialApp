# ----- RENDER CHECK DELETE -----
# This function is used to ensure that an rmarkdown file can be run without error.
# It will attempt to render the file, check if an output file was generated, 
# and if so, delete the output file
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
  list2env(load_test_data(), envir = environment())
  test_out_dir <- here::here("tests/temp")
  unlink(paste(test_out_dir,"/*", sep="")) # empty the folder
  
  catch_all_output({
    # set the title from render to hide warnings
    out_html <- rmarkdown::render(sectionPath, 
                                  output_options = list(pandoc_args = c("--metadata=title:\"test doc\"")),
                                  output_dir = test_out_dir)
    })
  
  success <- file.exists(out_html)
  if (file.exists(out_html) && deleteFile) {
    unlink(paste(test_out_dir,"/*", sep="")) # empty the folder
  }
  
  return(success)
}


# ----- Load Test Data -----
# This function is used to load necessary data into the environment for testing.
# Very similar to the initial chunks used in the first r markdown script.
#
# Inputs: None
#
# Outputs: list of variables to be loaded into the environment with list2env
#
# Developed by Quentin Stoyel, Summer 2021

load_test_data <- function() {
  catch_all_output({
    # check if globalenv data is loaded, if not load it:
    if (!exists("bioregion_sf")) {
      load(here::here("app/data/testData.RData"), envir = globalenv())
    }
    if (!exists("add_buffer")) {
      lapply(list.files(here::here("app/R"), pattern = ".[Rr]$", 
                        full.names = TRUE), source, local=globalenv())
    }
    
    # select random sample area from test sample areas
    studyAreaOpts <- here("app/studyAreaTest/", c("geoms_slc_coastal_test.geojson", 
                                                  "geoms_slc_no_land.geojson", 
                                                  "geoms_slc_coastal_test.geojson"))
    
    studyArea <- st_read(sample(studyAreaOpts, 1))
    site <- sf::st_centroid(studyArea)
    region <- st_read(here::here("app/studyAreaTest/geoms_slc_MarBioRegion.geojson"))
    
    mapDataList <- maps_setup(studyArea, site, region, land50k_sf, land10m_sf, bounds_sf)
    list2env(mapDataList, envir = environment())
  })
  
  outList <- list("minYear" = 2010,
                  "region" = region,
                  "site" = site,
                  "studyBox_geom" = studyBox_geom, 
                  "areaMap" = areaMap, 
                  "bboxMap" = bboxMap,
                  "regionBox" = regionBox, 
                  "regionMap" = regionMap,
                  "studyArea" = studyArea,
                  "mapDataList" = mapDataList)
  return(outList)
}


# ----- Catch all output -----
# Helper function that can suppress all warnings, messages and output
# Used to hide some the rmarkdown outputs when running tests as well as 
# some of the expected warnings.
#
# Inputs: codeChunk, all code to be run. Use function to wrap code that requires
#         output suppression.
#
# Outputs: None
#
# Developed by Quentin Stoyel, Summer 2021

catch_all_output <- function(codeChunk) {
  suppressWarnings({
    utils::capture.output({
      utils::capture.output({codeChunk}, file = "NUL", type = "output")
      }, file = "NUL", type = "message")
  })
}
