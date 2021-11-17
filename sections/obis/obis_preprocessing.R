source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

fileSavePath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\RData\\data\\MAR"
fileSavePath <- here::here("app/data/MAR")
fileLoadPath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data"

loadResult <- load_rdata(c("CommonData", "ebsa_rr"), "MAR")

#-------------------OBIS-------------------

# Legend file for displaying cetacean results in consistent colours
Legend <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/CetaceanLegend.csv"), stringsAsFactors = FALSE)
Legend <- dplyr::rename(Legend,c("Scientific Name" = "Scientific_Name"))
cetacean_list <- c("BELUGA WHALE", "NORTH ATLANTIC RIGHT WHALE", "FIN WHALE", 
                   "NORTHERN BOTTLENOSE WHALE",
                   "HARBOUR PORPOISE", "KILLER WHALE", "BLUE WHALE", "SEI WHALE", "SOWERBY'S BEAKED WHALE")


other_species_list <- c("LOGGERHEAD SEA TURTLE", "ATLANTIC WALRUS", "HARBOUR SEAL LACS DES LOUPS MARINS SUBSPECIES", "LEATHERBACK SEA TURTLE")
listed_cetacean_species <- subset(listed_species, COMMONNAME %in% cetacean_list)
listed_other_species <- subset(listed_species, COMMONNAME %in% other_species_list)
listed_fish_invert_species <- listed_species[ ! listed_species$COMMONNAME %in% c(other_species_list,cetacean_list), ]

# Ocean Biogeographic Information System (OBIS)
obis <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/OBIS_GBIF_iNaturalist/OBIS_MAR_priority_records.csv"), stringsAsFactors = FALSE)
obis <- dplyr::select(obis, scientificName, decimalLatitude, decimalLongitude, year)
obis <- obis %>% transmute(obis, scientificName = str_to_sentence(scientificName))
obis <- obis %>% rename("Scientific Name" = scientificName, "YEAR" = year)
obis <- obis %>% dplyr::filter(YEAR >= 2010)

# OBIS fish and inverts
obisFish <- merge(obis, listed_fish_invert_species, by='Scientific Name')
obisFish <- dplyr::select(obisFish, "Scientific Name", YEAR, "Common Name", "COSEWIC status",
                          "SARA status", decimalLatitude, decimalLongitude)
obisFish_sf <- st_as_sf(obisFish, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
obisFish_sf <- sf::st_crop(obisFish_sf, region_sf)

obisFish_rr <- list("title" = "OBIS observations",
                    "data_sf" = obisFish_sf,
                    "attribute" = "Legend",
                    "metadata" = list("contact" = email_format("helpdesk@obis.org"), 
                                      "url" = lang_list("<https://obis.org/>"),
                                      "accessedOnStr" = list("en" ="January 27 2021 by Gregory Puncher from OBIS", 
                                                             "fr" = "27 janvier 2021 par Gregory Puncher du SIBO") ,
                                      "accessDate" = as.Date("2021-01-27"),
                                      "searchYears" = "2010-2020",
                                      "securityLevel" = noneList,
                                      "qualityTier" = mediumQuality,
                                      "constraints" = noneList
                    )
)
save(obisFish_rr, file = file.path(fileSavePath, "Open/obisFish_rr.RData"))

# OBIS cetaceans
obisCet <- merge(obis, Legend, by='Scientific Name')
obisCet <- dplyr::select(obisCet, "Scientific Name", YEAR, Legend,
                         decimalLatitude, decimalLongitude)
obisCet_sf <- st_as_sf(obisCet, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
obisCet_sf <- sf::st_crop(obisCet_sf, region_sf)


obisCet_rr <- list("title" = "OBIS observations (cetaceans)",
                   "data_sf" = obisCet_sf,
                   "attribute" = "Legend",
                   "metadata" = list("contact" = email_format("helpdesk@obis.org"), 
                                     "url" = lang_list("<https://obis.org/>"),
                                     "accessedOnStr" = list("en" ="January 27 2021 by Gregory Puncher from OBIS", 
                                                            "fr" = "27 janvier 2021 par Gregory Puncher du SIBO") ,
                                     "accessDate" = as.Date("2021-01-27"),
                                     "searchYears" = "2010-2020",
                                     "securityLevel" = noneList,
                                     "qualityTier" = mediumQuality,
                                     "constraints" = noneList
                   )
)
save(obisCet_rr, file = file.path(fileSavePath, "Open/obisCet_rr.RData"))

