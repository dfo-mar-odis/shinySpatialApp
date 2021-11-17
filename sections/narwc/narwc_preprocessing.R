source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

fileSavePath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\RData\\data\\MAR"
fileSavePath <- here::here("app/data/MAR")
fileLoadPath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data"

loadResult <- load_rdata(c("CommonData", "narwc_rr"), "MAR")

# ----------------------NARWC-------------------------
# North Atlantic Right Whale Consortium (narwc)
narwc <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/NARWC/NARWC_09-18-2020.csv"), stringsAsFactors = FALSE)
narwcspecies <-  read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/NARWC/NARWCSpeciesNames.csv"), stringsAsFactors = FALSE)
narwcspecies <- narwcspecies %>% rename("Scientific Name"= ScientificName)
narwc <- merge(narwc, narwcspecies, by='SPECNAME')
narwc <- narwc %>% dplyr::filter(YEAR >= 2010)
narwc <- merge(narwc, cetLegend, by = 'Scientific Name')
narwc <- dplyr::select(narwc, 'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
narwc_sf <- st_as_sf(narwc, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
narwc_sf <- sf::st_crop(narwc_sf, region_sf)

narwc_rr <- list("title" = "North Atlantic Right Whale consortium",
                 "data_sf" = narwc_sf,
                 "attribute" = "Legend",
                 "metadata" = list("contact" = "<hpettis@neaq.org>", 
                                   "url" = lang_list("<https://www.narwc.org/sightings-database.html>"),
                                   "accessedOnStr" = list("en" ="September 18 2020", "fr" = "18 septembre 2020") ,
                                   "accessDate" = as.Date("2020-09-18"),
                                   "searchYears" = "2010-2019",
                                   "securityLevel" = noneList,
                                   "qualityTier" = highQuality,
                                   "constraints" = internalUse
                 )
)
save(narwc_rr, file = file.path(fileSavePath, "Secure/narwc_rr.RData"))