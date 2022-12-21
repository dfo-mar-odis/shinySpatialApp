
source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))



# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "waste_rr"), regionStr)

print("----------------Waste----------------- ")
if (globalControlEnv$updateGeoms) {


wastePkgId <- "da99526e-284f-4e06-8d04-193785cd1a96"

waste_rr <- get_opendata_rr(wastePkgId)

url <- "https://maps-cartes.ec.gc.ca/arcgis/rest/services/Active_Inactive_Disposal_at_Sea_Sites/MapServer/"
#where <- "SCAT_Class_EN = 'Man-Made Solid'"
waste_sf <- get_esri_rest(url, layer="0")
waste_rr$data_sf <- sf::st_crop(waste_sf, region_sf) %>% 
  sf::st_make_valid()
# already in 4326.

}

waste_rr$metadata <- read_google_metadata("waste_rr", isOpenData = TRUE)

save(waste_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/waste_rr.RData"))
