# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))

library(lubridate) # load library to use year/month/day as functions and headers

# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "snowCrabTrawl_rr"), regionStr)

print(" ---------------------Snow Crab Trawl-----------------------------------")
if (globalControlEnv$updateGeoms) {
    
  dat <- read.csv("R:/Science/BIODataSvc/IN/MSP/Data/NaturalResources/Species/SnowCrabTrawl/snowcrabdat23.csv", header = TRUE)
  dat_sf <- dat %>%
    dplyr::select(year, month, AREA, STATION, SLONG, SLAT) %>% #select required columns
    distinct() %>%
    sf::st_as_sf(coords=c("SLONG", "SLAT"), crs=4326) %>% #set coordinate system
    dplyr::filter(year > 2012)
  
  labels <- dat_sf %>% 
    dplyr::group_by(AREA) %>%
    summarize() %>% 
    sf::st_centroid() %>%
    cbind(st_coordinates(.))

} else {
  dat_sf <- snowCrabTrawl_rr$data_sf
}


# rr object structure:
snowCrabTrawl_rr <- list(
  "title" = "Snow Crab Trawl Survey",
  "data_sf" = dat_sf,
  "attribute" = "NONE",
  "metadata" = read_google_metadata("snowCrabTrawl_rr")
  )
save(snowCrabTrawl_rr, labels, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Protected/snowCrabTrawl_rr.RData"))

