# contains functions for downloading open data records
source(here::here("dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("app/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "whelk_rr"), regionStr)

# ---------------------WHELK SURVEY-----------------------------------
# Open Data dataset
pkgId <- "e606ad01-843f-4730-93fa-f944dfbbbf07"
resId <- "5355ea5b-ea37-4a4b-8f61-9d6b716b7d17"

# get open data metadata
whelk_rr <- get_opendata_rr(pkgId, NULL)

# download csv's from open data
outData <- download_extract_res_files(resId, c("taxon_occurrence", "event_information"))
occuranceData <- data.frame(outData[1])
eventData <- data.frame(outData[2])

# combineing the data
allData <- left_join(occuranceData, eventData, by="eventID", keep=TRUE)

trimmedData <- dplyr::select(allData, c("scientificName", "year", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "minimumDepthInMeters", "maximumDepthInMeters"))
trimmedData <- dplyr::filter(trimmedData, year > rrMinYear)

# converting to an sf and processing
whelk_sf <- sf::st_as_sf(trimmedData, coords=c("decimalLongitude", "decimalLatitude"), crs = 4326)
whelk_sf <- st_make_valid(whelk_sf)
whelk_sf <- sf::st_crop(whelk_sf, region_sf)

# adding sf to the rr object and saving
whelk_rr$data_sf <- whelk_sf
save(whelk_rr, file = file.path(localFileSavePath, "Open/whelk_rr.RData"))


# OBIS Dataset
library("robis") 
# reqeusting obis data
obisWhelk <- robis::occurrence(datasetid = "eae87607-a740-4f85-abf8-2af89764355b")
trimmedData <- dplyr::select(obisWhelk, c("scientificName", "year", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "minimumDepthInMeters", "maximumDepthInMeters"))
trimmedData <- dplyr::filter(trimmedData, year > rrMinYear)

# convert to sf, processing:
whelk_sf <- sf::st_as_sf(trimmedData, coords=c("decimalLongitude", "decimalLatitude"), crs = 4326)
whelk_sf <- st_make_valid(whelk_sf)
whelk_sf <- sf::st_crop(whelk_sf, region_sf)
whelk_rr$data_sf <- whelk_sf

# rr object structure:
whelk_rr <- list("title" = "Biodiversity of the whelk (Buccinum) dredge survey in the St. Lawrence Estuary",
              "data_sf" = whelk_sf,
              "attribute" = "NONE",
              "metadata" = list("contact" = email_format("virginie.roy@dfo-mpo.gc.ca"), 
                                "accessedOnStr" = list("en" ="January 24, 2022", "fr" = "24 Janvier, 2022") ,
                                "accessDate" = as.Date("2022-01-04"),
                                "searchYears" = paste(rrMinYear,"-2022", sep=""),
                                "securityLevel" = noneList,
                                "qualityTier" = highQuality,
                                "constraints" = internalUse
              )
)
save(whelk_rr, file = file.path(localFileSavePath, "Open/whelk_rr.RData"))

