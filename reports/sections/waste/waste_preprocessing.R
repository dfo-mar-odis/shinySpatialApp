# contains functions for downloading open data records
source(here::here("dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("app/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "waste_rr"), regionStr)

# ----------------Waste----------------- 
wastePkgId <- "da99526e-284f-4e06-8d04-193785cd1a96"

waste_rr <- get_opendata_rr(wastePkgId)

url <- "https://maps-cartes.ec.gc.ca/arcgis/rest/services/Active_Inactive_Disposal_at_Sea_Sites/MapServer"
#where <- "SCAT_Class_EN = 'Man-Made Solid'"
waste_sf <- get_esri_rest(url, layer="0")
waste_rr$data_sf <- sf::st_crop(waste_sf, region_sf) %>% 
  sf::st_make_valid()
# already in 4326.

waste_rr$attribute <- "Status"
waste_rr$metadata$contact <- paste(email_format("michael.parry@canada.ca"),  email_format("Michael.Buckland-Nicks@canada.ca"), sep=", ")
waste_rr$metadata$searchYears <- "2018"
waste_rr$metadata$qualityTier <- mediumQuality
save(waste_rr, file = file.path(localFileSavePath, "Open/waste_rr.RData"))
