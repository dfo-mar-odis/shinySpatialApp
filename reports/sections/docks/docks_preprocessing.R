# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))

# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "docks_rr"), regionStr)

# ---------------------TEMPLATE-----------------------------------
if (globalControlEnv$updateGeoms) {
  
  docksPkgId <- "30449352-2556-42df-9ffe-47ea8e696f91"
  
  docks_rr <- get_opendata_rr(docksPkgId)
  
  url <- "https://ec.gc.ca/arcgis/rest/services/EPB_EPO/ShorelineSegmentationWithSCATClassification/MapServer"
  where <- "SCAT_Class_EN = 'Man-Made Solid'"
  docks_sf <- get_esri_rest(url, layer="4", where=where)
  docks_rr$data_sf <- sf::st_crop(docks_sf, region_sf) %>% 
    sf::st_cast("MULTILINESTRING") %>% 
    sf::st_make_valid()
  # already in 4326 and cropped.
  
  docks_rr$attribute <- "NONE"
  docks_rr$title$en <- "Man-made solid classification from the Atlantic Shoreline Classification dataset"
}

docks_rr$metadata <- read_google_metadata("docks_rr")

save(docks_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/docks_rr.RData"))

