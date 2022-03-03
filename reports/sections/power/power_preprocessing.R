# contains functions for downloading open data records
source(here::here("dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("app/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "power_rr"), regionStr)

# ----------------Power----------------- 
powerPkgId <- "65d3db23-b83c-4f49-ab93-65c59ee0e6aa"

power_rr <- get_opendata_rr(powerPkgId)

url <- "https://geoappext.nrcan.gc.ca/arcgis/rest/services/Energy/clean_energy_generating_stations_fgp/MapServer"
# only take stations east of Thunder Bay, crop to region later
where <- "Longitude > -90"
power_sf <- get_esri_rest(url, layer="0", where=where)
power_rr$data_sf <- st_crop(power_sf, region_sf) %>% 
  st_make_valid()
# already in 4326.

power_rr$attribute <- "MEGAWATTS"
power_rr$metadata$contact <- email_format("NRCan.geogratis-geogratis.RNCan@canada.ca")
power_rr$metadata$searchYears <- "2016"
power_rr$metadata$qualityTier <- mediumQuality
save(power_rr, file = file.path(localFileSavePath, "Open/power_rr.RData"))