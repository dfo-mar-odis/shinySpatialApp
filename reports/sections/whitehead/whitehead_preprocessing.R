source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "whitehead_rr"), regionStr)

print("----------------WHITEHEAD-------------- ")
if (globalControlEnv$updateGeoms) {

# Whitehead lab
whitehead <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/Whitehead_Lab/whitehead_lab.csv"), stringsAsFactors = FALSE)
whitehead$YEAR <- lubridate::year(whitehead$Date)
whitehead <- whitehead %>% dplyr::filter(YEAR >= rrMinYear)
whitehead <- whitehead %>% dplyr::rename("Scientific Name"= species.name)
whitehead <- merge(whitehead, cetLegend, by='Scientific Name')
whitehead <- dplyr::select(whitehead, 'Scientific Name', YEAR, Legend, Lat, Long)
# correct the longitude values to be negative
whitehead$Long <- -1 * whitehead$Long
whitehead_sf <- sf::st_as_sf(whitehead, coords = c("Long", "Lat"), crs = 4326)
whitehead_sf <- sf::st_crop(whitehead_sf, region_sf)

}

whitehead_rr$metadata <- read_google_metadata("whitehead_rr", isOpenData = FALSE)

save(whitehead_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Secure/whitehead_rr.RData"))