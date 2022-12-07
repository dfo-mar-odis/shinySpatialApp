# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "whelk_rr"), regionStr)

print("---------------------WHELK SURVEY----------------------------------- ")
if (globalControlEnv$updateGeoms) {

# Open Data dataset
whelkpkgId <- "e606ad01-843f-4730-93fa-f944dfbbbf07"
resId <- "5355ea5b-ea37-4a4b-8f61-9d6b716b7d17"

# get open data metadata
whelk_rr <- get_opendata_rr(whelkpkgId)

#This chunk will pop a warning because none of the points are within Maritimes region#
esriBase <- "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/Biodiversity_of_the_whelk_Buccinum_dredge_survey_in_the_St_Lawrence_Estuary_En/MapServer/"
esriUrl <- paste0(esriBase, "0")
regionBbox <- sf::st_bbox(sf::st_transform(region_sf, 3857))
data_sf <- esri2sf::esri2sf(esriUrl, bbox=regionBbox, progress = TRUE)
whelk_rr$data_sf <- sf::st_make_valid(data_sf)

# combining the data
allData <- left_join(occuranceData, eventData, by="eventID", keep=TRUE)

trimmedData <- dplyr::select(allData, c("scientificName", "year", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "minimumDepthInMeters", "maximumDepthInMeters"))
trimmedData <- dplyr::filter(trimmedData, year > rrMinYear)

# converting to an sf and processing
whelk_sf <- sf::st_as_sf(trimmedData, coords=c("decimalLongitude", "decimalLatitude"), crs = 4326)
whelk_sf <- sf::st_make_valid(whelk_sf)
whelk_sf <- sf::st_crop(whelk_sf, region_sf)

# adding sf to the rr object and saving
whelk_rr$data_sf <- whelk_sf
}

whelk_rr$metadata <- read_google_metadata("whelk_rr", isOpenData = TRUE)

save(wsdb_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/whelk_rr.RData"))

