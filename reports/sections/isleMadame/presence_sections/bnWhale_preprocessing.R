# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "bnWhale_rr"), regionStr)

# ---------------------Bottlenose Whale Presence-----------------------------------
pkgId <- "29dd835b-7c96-4c62-b558-275dfe13cbe9"
bnWhale_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_BottlenoseWhale/MapServer/"
bnWhale_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
bnWhale_sf <- dplyr::select(bnWhale_sf, c("OVERALL_PRESENCE", "LIFE_STAGE", "RELATIVE_DISTRIBUTION",
                                          "RELATIVE_ABUNDANCE", "IMPORTANCE_RATIONALE", "JAN", "FEB",
                                          "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT",
                                          "NOV", "DEC"))
bnWhale_sf$OVERALL_PRESENCE[bnWhale_sf$OVERALL_PRESENCE == "see monthly presence"] <- "Verify with original record"



bnWhale_rr$data_sf <- sf::st_transform(bnWhale_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)

bnWhale_rr$attribute <- "None"
bnWhale_rr$metadata <- read_google_metadata("Many")
bnWhale_rr$datasetName <- "Likelihood of Presence of Bottlenose Whales in Area Response Planning Pilot Areas"

save(bnWhale_rr, file = file.path(localFileSavePath, "Open/bnWhale_rr.RData"))
