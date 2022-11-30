# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "imSalmonRivers_rr"), regionStr)

# ---------------------Presence-----------------------------------
pkgId <- "ded53eaa-bb98-4476-beea-3138372c740b"
imSalmonRivers_rr <- get_opendata_rr(pkgId)

esriUrl <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/OPP_ARP_MAR_SpeciesPresence_SalmonRivers_En/MapServer/"
imSalmonRivers_sf <- esri2sf::esri2sf(paste0(esriUrl, "0"), progress = TRUE)
names(imSalmonRivers_sf)[names(imSalmonRivers_sf) == 'geoms'] <- 'geometry'
st_geometry(imSalmonRivers_sf) <- "geometry"

imSalmonRivers_rr$data_sf <- sf::st_transform(imSalmonRivers_sf, crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_crop(region_sf)


imSalmonRivers_rr$attribute <- "DU"
imSalmonRivers_rr$metadata$qualityTier <- mediumQuality
imSalmonRivers_rr$metadata$pipelinePath <- paste0(githubRepo, "reports/sections/isleMadame/presence_sections/imSalmonRivers_preprocessing.R")
imSalmonRivers_rr$metadata$constraints <- list("en" = "For environmental response use only", "fr" = "For environmental response use only")
imSalmonRivers_rr$datasetName <- "Salmon Rivers Presence in Area Response Planning Pilot Areas"
save(imSalmonRivers_rr, file = file.path(localFileSavePath, "Open/imSalmonRivers_rr.RData"))
