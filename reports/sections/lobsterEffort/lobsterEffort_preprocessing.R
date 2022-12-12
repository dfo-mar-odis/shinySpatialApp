source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))

# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "lobsterEffort_rr"), regionStr)

print("---------------------LobsterEffort-----------------------------------")
if (globalControlEnv$updateGeoms) {
    
  lobsterEffortPkgId <- "64f741d7-1129-49dd-9e5c-2b1de79024f0"
  
  lobsterEffort_rr <- get_opendata_rr(lobsterEffortPkgId)
  
  esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Maritimes_Inshore_Lobster_Landings_Effort_2012_2014_EN/MapServer/"
  esriUrl <- paste0(esriBase, "1")
  regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 2961))
  data_sf <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, progress = TRUE)
  lobsterEffort_rr$data_sf <- sf::st_make_valid(data_sf)
  # already in 4326 and cropped.
}

lobsterEffort_rr$attribute <- "weight_kg" 
lobsterEffort_rr$metadata <- read_google_metadata("lobsterEffort_rr")

save(lobsterEffort_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/lobsterEffort_rr.RData"))

