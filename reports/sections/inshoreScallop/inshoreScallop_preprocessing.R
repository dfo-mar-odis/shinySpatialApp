# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))

library(lubridate) # load library to use year/month/day as functions and headers

# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "inshoreScallop_rr"), regionStr)

print(" ---------------------Inshore Scallop-----------------------------------")
if (globalControlEnv$updateGeoms) {
    
  #load("R:/Science/BIODataSvc/IN/MSP/Data/NaturalResources/Species/InshoreScallops/ISdat.RData")
dat <- read.csv("R:/Science/BIODataSvc/IN/MSP/Data/NaturalResources/Species/InshoreScallops/ISdat.csv", header = TRUE)

  dat_sf <- dat %>%
    dplyr::select(year, month, AREA, TOW_NO, SLONG, SLAT) %>% #select required columns
    distinct() %>%
    sf::st_as_sf(coords=c("SLONG", "SLAT"), crs=4326) %>% #set coordinate system
    dplyr::filter(year > 2012) #Trim data to the 10 year window
    #sf::st_intersection(region_sf) #trim to region #This line is not working
  
  areaNames <- data.frame(AREA = c("Bay of Fundy", "Bay of Fundy Approach", "Brier/Lurcher", "Annapolis Basin", "Grand Manan", "SFA 29 West"), 
                          name = c("Bay of Fundy", "Bay of Fundy Approach", "Brier/Lurcher", "Annapolis Basin", "Grand Manan", "SFA 29 West"))
  
  
  #create labels for the plot
  areaLabels <- dat_sf %>% 
    dplyr::group_by(AREA) %>%
    summarize() %>% 
    sf::st_centroid() %>%
    cbind(st_coordinates(.)) %>% 
    dplyr::left_join(areaNames)
} else {
  dat_sf <- inshoreScallop_rr$data_sf
}


# rr object structure:
inshoreScallop_rr <- list(
  "title" = "Inshore Scallop Survey",
  "data_sf" = dat_sf,
  "attribute" = "NONE",
  "metadata" = read_google_metadata("inshoreScallop_rr")
  )
save(inshoreScallop_rr, areaLabels, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Protected/inshoreScallop_rr.RData"))

