source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "calanus_rr"), regionStr)

print("----------------calanus----------------- ")
if (globalControlEnv$updateGeoms) {
    
  calanusPkgId <- "72e6d3a1-06e7-4f41-acec-e0f1474b555b"
  calanus_rr <- get_opendata_rr(calanusPkgId)
  
  esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Calanus_spp_Size_and_Lipid_Content_Metrics_in_North_Atlantic_1977_2019/MapServer/"
  esriUrl <- paste0(esriBase, "0")
  
  regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
  data_sf <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, progress = TRUE)
  data_sf <- sf::st_make_valid(data_sf)
  data_sf <- dplyr::filter(data_sf, Year > rrMinYear)
  calanus_rr$data_sf <- unique(dplyr::select(data_sf, c('Year', "Station", 'geoms')))
}
calanus_rr$metadata <- read_google_metadata("ebsa_rr", isOpenData = TRUE)

save(calanus_rr, file = file.path(localFileSavePath, "Open/calanus_rr.RData"))
