source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))


loadResult <- load_rdata(c("CommonData", "substrate_rr"), regionStr)

print("----------------substrate----------------- ")
if (globalControlEnv$updateGeoms) {
    
  substratePkgId <- "f2c493e4-ceaa-11eb-be59-1860247f53e3"
  
  substrate_rr <- get_opendata_rr(substratePkgId)
  
  esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Substrate_Classification_Inshore_Scotian_Shelf_and_Bay_of_Fundy_Maritimes/MapServer/"
  esriUrl <- paste0(esriBase, "0")
  regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
  firstThird <- "OBJECTID < 5"
  secondThird <- "OBJECTID IN (5,6,7)"
  thirdThird <- "OBJECTID > 7"
  data_sf1 <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, where = firstThird, progress = TRUE)
  data_sf2 <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, where = secondThird, progress = TRUE)
  data_sf3 <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, where = thirdThird, progress = TRUE)
  data_sf <- rbind(data_sf1,data_sf2)
  data_sf <- rbind(data_sf, data_sf3)
  substrate_rr$data_sf <- sf::st_make_valid(data_sf)
}

substrate_rr$metadata <- read_google_metadata("substrate_rr", isOpenData = TRUE)

substrate_rr$attribute = "Substrate"

save(substrate_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/substrate_rr.RData"))


