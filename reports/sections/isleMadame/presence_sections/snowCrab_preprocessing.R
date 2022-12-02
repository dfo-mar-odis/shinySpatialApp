# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "snowCrab_rr"), regionStr)

# ---------------------Bluefin Tuna Presence-----------------------------------
pkgId <- "edb15c7b-d901-46b0-a460-1aca22c013ea"
snowCrab_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_SnowCrab/MapServer/"
snowCrab_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
snowCrab_sf <- dplyr::select(snowCrab_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                    "RELATIVE_ABUNDANCE", "GEOGRAPHIC_AREA", "IMPORTANCE_RATIONALE", "JAN", "FEB",
                                    "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT",
                                    "NOV", "DEC"))
snowCrab_sf$OVERALL_PRESENCE[snowCrab_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Verify with original record"
snowCrab_sf["Overall Presence"] <- snowCrab_sf$OVERALL_PRESENCE



snowCrab_rr$data_sf <- sf::st_transform(snowCrab_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


snowCrab_rr$attribute <- "Overall Presence"
snowCrab_rr$metadata <- read_google_metadata("Many")
snowCrab_rr$datasetName <- "Likelihood of Presence of Snow Crab in Area Response Planning Pilot Areas"
save(snowCrab_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/snowCrab_rr.RData"))
