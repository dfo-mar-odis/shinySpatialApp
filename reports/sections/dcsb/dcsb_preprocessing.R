source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))


loadResult <- load_rdata(c("CommonData", "dcsb_rr"), regionStr)

print("----------------DCSB----------------- ")
if (globalControlEnv$updateGeoms) {
    
  dcsbPkgId <- "6af357a3-3be1-47d5-9d1f-e4f809c4c903"
  
  dcsb_rr <- get_opendata_rr(dcsbPkgId)
  
  esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/SBAs_Coral_Sponge_2016_EN/MapServer/"
  esriUrl <- paste0(esriBase, "0")
  regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
  data_sf <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, progress = TRUE)
  dcsb_rr$data_sf <- sf::st_make_valid(data_sf)
  dcsb_rr$data_sf$Label_Name <- as.factor(dcsb_rr$data_sf$Label_Name)
  dcsb_rr$attribute <- "Label_Name"
}

dcsb_rr$metadata <- read_google_metadata("dcsb_rr", isOpenData = TRUE)

save(dcsb_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/dcsb_rr.RData"))


