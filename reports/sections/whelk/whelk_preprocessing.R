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
  
  # get open data metadata
  whelk_rr <- get_opendata_rr(whelkpkgId)
  
  # download csv's from open data  
  resId <- "5355ea5b-ea37-4a4b-8f61-9d6b716b7d17"
  outData <- download_extract_res_files(resId, c("taxon_occurrence", "event_information"))
  occuranceData <- data.frame(outData[1])
  eventData <- data.frame(outData[2])
  
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

whelk_rr$metadata$url <- lang_list(NA)
whelk_rr$metadata$contact <- lang_list(NA)
whelk_rr$metadata$searchYears <- NA
whelk_rr$metadata$reference <- lang_list(NA)
whelk_rr$metadata$pipelinePath = paste0(githubRepo, "reports/sections/whelk/whelk_preprocessing.R")
save(whelk_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/whelk_rr.RData"))

