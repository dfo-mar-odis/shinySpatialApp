source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))

# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "power_rr"), regionStr)

print(" ----------------Power----------------- ")
if (globalControlEnv$updateGeoms) {
  
  powerPkgId <- "65d3db23-b83c-4f49-ab93-65c59ee0e6aa"
  
  power_rr <- get_opendata_rr(powerPkgId)
  
  url <- "https://geoappext.nrcan.gc.ca/arcgis/rest/services/Energy/clean_energy_generating_stations_fgp/MapServer/"
  # only take stations east of Thunder Bay, crop to region later
  esriUrl <- paste0(url, "0")
  regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3978))
  data_sf <- esri2sf::esri2sf(esriUrl,  bbox=regionBbox, progress = TRUE)
  power_rr$data_sf <- sf::st_make_valid(data_sf)
  # already in 4326.
}

power_rr$attribute <- "MEGAWATTS"
power_rr$metadata <- read_google_metadata("power_rr", isOpenData = TRUE)

save(power_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/power_rr.RData"))
