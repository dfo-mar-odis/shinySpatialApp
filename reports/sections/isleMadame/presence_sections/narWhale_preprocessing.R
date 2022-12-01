# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "narWhale_rr"), regionStr)

# ---------------------Bluefin Tuna Presence-----------------------------------
pkgId <- "d159ac68-6e46-44b5-a4e9-951880892c63"
narWhale_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_NARightwhale/MapServer/"
narWhale_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
narWhale_sf <- dplyr::select(narWhale_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                    "RELATIVE_ABUNDANCE", "GEOGRAPHIC_AREA", "IMPORTANCE_RATIONALE", "JAN", "FEB",
                                    "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT",
                                    "NOV", "DEC"))
narWhale_sf$OVERALL_PRESENCE[narWhale_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Verify with original record"



narWhale_rr$data_sf <- sf::st_transform(narWhale_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


narWhale_rr$attribute <- "None"
narWhale_rr$metadata <- read_google_metadata("Many")
narWhale_rr$datasetName <- "Likelihood of Presence of North Atlantic Right Whales in Area Response Planning Pilot Areas"
save(narWhale_rr, file = file.path(localFileSavePath, "Open/narWhale_rr.RData"))
