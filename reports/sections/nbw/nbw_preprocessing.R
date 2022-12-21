source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "nbw_rr"), regionStr)

print("-----------NBW-------------- ")
if (globalControlEnv$updateGeoms) {
  nbwPkgId <- "9fd7d004-970c-11eb-a2f3-1860247f53e3"

  nbw_rr <- get_opendata_rr(nbwPkgId)
  
  esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Northern_Bottlenose_Whale_Important_Habitat_Eastern_Scotian_Shelf/MapServer/"
  esriUrl <- paste0(esriBase, "0")
  regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
  data_sf <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, progress = TRUE)
  nbw_rr$data_sf <- sf::st_make_valid(data_sf)
}

nbw_rr$metadata <- read_google_metadata("nbw_rr", isOpenData = TRUE)


save(nbw_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/nbw_rr.RData"))
