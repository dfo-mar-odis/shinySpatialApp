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

render_check_delete <- function(section_path, delete_file=TRUE) {
  test_out_dir = here("app/tests/temp")
  unlink(test_out_dir) # empty the folder
  out_html <- rmarkdown::render(section_path, output_dir = test_out_dir)
  success <- file.exists(out_html)
  if (file.exists(out_html) & delete_file) {
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


load_test_data <- function() {
  minYear <<- 2010
  toenv <- globalenv() # loads the data to environment where function is called
  load(here("app/data/OpenData.RData"), envir=toenv)
  load(here("app/data/OpenData_sardist.RData"), envir=toenv)
  load(here("app/data/SecureData.RData"), envir=toenv)
  lapply(list.files(here("app/R"), pattern = ".[Rr]$", full.names = TRUE), source, local=toenv)
}

# ----- PREP TEST DATA -----
# This function is used to prepare the initial data needed for rendering rmd files.
# Once the studyArea is known, it produces the needed generic plots and data used throughout the rmd files.
#
# Inputs:
# 1. studyArea simple feature delimiting the study area of interest.    
#
# Outputs: 
# 1. out_list, a list of values that need to be set in the environment, can be done with: list2env(prepped_data, env=environment())
#
# Developed by Quentin Stoyel, Summer 2021


prep_test_data <- function(studyArea) {
  site <- sf::st_centroid(studyArea)
  
  studyBox_geom <- geom_sf(data=studyArea, fill=NA, col="red", size=1) 
  
  areaMapList <- area_map(studyArea, site, land50k_sf, 5, bounds_sf, studyBox_geom)
  areaMap <- areaMapList[[1]] # map
  bboxMap <- areaMapList[[2]] #bounding box of the map
  
  Region <- st_read(here("app/studyAreaTest/geoms_slc_MarBioRegion.geojson"))
  regionBox <- sf::st_bbox(Region) 
  regionMap <- region_map(regionBox, studyArea, land10m_sf, bounds_sf)  
  out_list <- list("Region" = Region,
                   "site" = site,
                   "studyBox_geom" = studyBox_geom, 
                   "areaMap" = areaMap, 
                   "bboxMap" = bboxMap,
                   "regionBox" = regionBox, 
                   "regionMap" = regionMap,
                   "studyArea" = studyArea)
  return(out_list)
}

