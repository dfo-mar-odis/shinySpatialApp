source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "ebsa_rr"), regionStr)

print("----------------EBSA----------------- ")
if (globalControlEnv$updateGeoms) {
    
  ebsaPkgId <- "d2d6057f-d7c4-45d9-9fd9-0a58370577e0"
  
  ebsa_rr <- get_opendata_rr(ebsaPkgId)
  
  esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Ecologically_and_Biologically_Significant_Areas/MapServer/"
  esriUrl <- paste0(esriBase, "0")
  regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
  data_sf <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, progress = TRUE)
  ebsa_rr$data_sf <- sf::st_make_valid(data_sf)
}

ebsa_rr$metadata <- read_google_metadata("ebsa_rr", isOpenData = TRUE)

save(ebsa_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/ebsa_rr.RData"))


