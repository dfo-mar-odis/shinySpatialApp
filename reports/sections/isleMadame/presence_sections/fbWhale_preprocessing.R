# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "fbWhale_rr"), regionStr)

# ---------------------Fin Whale Presence-----------------------------------
pkgId <- "7e2f85b3-19eb-4ecf-8557-69c8df1bc084"
fbWhale_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_FinbackWhale/MapServer/"
fbWhale_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
fbWhale_sf <- dplyr::select(fbWhale_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                    "RELATIVE_ABUNDANCE", "GEOGRAPHIC_AREA", "IMPORTANCE_RATIONALE", "JAN", "FEB",
                                    "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT",
                                    "NOV", "DEC"))
fbWhale_sf$OVERALL_PRESENCE[fbWhale_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Verify with original record"



fbWhale_rr$data_sf <- sf::st_transform(fbWhale_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


fbWhale_rr$attribute <- "None"

fbWhale_rr$metadata <- read_google_metadata("Many")

fbWhale_rr$datasetName <- "Likelihood of Presence of Finback Whales in Area Response Planning Pilot Areas"
save(fbWhale_rr, file = file.path(localFileSavePath, "Open/fbWhale_rr.RData"))
