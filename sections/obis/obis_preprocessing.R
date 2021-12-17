source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "obisCet_rr", "obisFish_rr"), regionStr)

#-------------------OBIS-------------------
##############################################################################################################################
# Downloading Ocean Biodiversity Information System (OBIS) data for the Reproducible Report: DRAFT
# Created by Stephen Finnis, Quentin Stoyel, and Catalina Gomez in November 2021
# Code will download data from OBIS within the Scotian Shelf Bioregion
# Will obtain records for fish and invertebrate species, and cetaceans listed by the Species at Risk Act and/or
# assessed by the Committee on the Status of Endangered Wildlife in Canada
##############################################################################################################################
# Get occurrence data for these data within the Scotian Shelf Bioregion (approximate location) from 2010 to present
# This may take a few minutes to run
regionWKT <- sf::st_as_text(region_sf$geometry)

occurrenceNS = occurrence(scientificname=listed_species$`Scientific Name`, 
                          geometry = regionWKT, startdate = "2010-01-01")

# Get the dataset information for the each of the occurrence records above
# Most of the dataset information has to be accessed separately from the occurrence data 
# This is due to the way the information is stored in OBIS (known as the Darwin Core Standard)
datasetNS = dataset(datasetid = unique(occurrenceNS$dataset_id))

##############################################################################################################################
## Prepare and combine data into appropriate format

# Want lat/lon data to be written in Well-Known Text (WKT) format
# Most of the footprintWKT column is already in the correct WKT format
# Some point data have NAs. These need to be replaced. They should begin with "POINT(" 
# and close with ")". I don't know why POINT data have one bracket, but POLYLINES have two
occurrenceNS$updateFootprint = ifelse(is.na(occurrenceNS$footprintWKT)=="TRUE",
                                      paste0("POINT(",paste(occurrenceNS$decimalLatitude, occurrenceNS$decimalLongitude),")"),
                                      occurrenceNS$footprintWKT)

# Combine the occurrence and dataset dataframes based on their dataset IDs
occDatasetNS = full_join(occurrenceNS, datasetNS, by=c("dataset_id"="id"))

# Then combine with the MAR species spreadsheet to get other relevant info (e.g., SARA/COSEWIC schedules, and Schedule status)
comboDataNS = full_join(occDatasetNS, listed_species, by=c("scientificName" = "Scientific Name"))

# Remove records from datasets that are already included in the report
comboDataNS.remove = subset(comboDataNS, title!="Maritimes Summer Research Vessel Surveys" & 
                              title!= "Maritimes Spring Research Vessel Surveys"& 
                              title!= "Maritimes 4VSW Research Vessel Surveys" & 
                              title!= "DFO Maritimes Region Cetacean Sightings"& # aka Whale Sightings Database
                              dataset_id!= "NA") # these represent species that didn't have any OBIS occurrences but got added when joined with the MAR species spreadsheet

# Select the required columns. We may decide later that more are useful
obis_df = dplyr::select(comboDataNS.remove, scientificName, "Common Name", 
                   COSEWIC.population, date_year, url,
                   citation, title, "COSEWIC status", SARA.schedule, 
                   basisOfRecord, flags, eventDate, 
                   basisOfRecord, shoredistance, decimalLatitude, 
                   decimalLongitude)

# Separate out the Machine Observations records for making tables/figures
machineobs = subset(alldataNS, basisOfRecord=="MachineObservation")
everythingelse = subset(alldataNS, basisOfRecord!="MachineObservation") # exclude machine observation records

# Things I still need to do:
# Separate fish/invertebrates from cetaceans (this should also remove sea turtle records)
# Once that is done, exclude cetacean data that are >300m from shore i.e. "shoredistance < -300"
obis_sf <- st_as_sf(obis_df, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# split off the cetaceans, cetLegend and rr_otherspecies are from common data
obis_sf <- dplyr::rename(obis_sf, "Scientific Name"="scientificName")

obisCet_sf <- subset(obis_sf, obis_sf$`Scientific Name` %in% cetLegend$`Scientific Name`)
obisCet_sf <- merge(obisCet_sf, cetLegend, by='Scientific Name')
obisFish_sf <- subset(obis_sf, !(obis_sf$`Scientific Name` %in% cetLegend$`Scientific Name`)
                      & !(obis_sf$`Scientific Name` %in% rr_otherSpecies$Scientific_Name))

obisCet_rr <- list("title" = "Ocean Biodiversity Information System (OBIS), Cetaceans",
                   "data_sf" = obisCet_sf,
                   "attribute" = "Legend",
                   "metadata" = list("contact" = email_format("helpdesk@obis.org"), 
                                     "url" = lang_list("<https://obis.org/>"),
                                     "accessedOnStr" = list("en" ="December 17 2021 using robis", 
                                                            "fr" = "17 decmbre 2021 par robis") ,
                                     "accessDate" = as.Date("2021-12-17"),
                                     "searchYears" = "2010-2021",
                                     "securityLevel" = noneList,
                                     "qualityTier" = variableQuality,
                                     "constraints" = noneList
                   )
)
save(obisCet_rr, file = file.path(localFileSavePath, "Open/obisCet_rr.RData"))

obisFish_rr <- list("title" = "Ocean Biodiversity Information System (OBIS)",
                    "data_sf" = obisFish_sf,
                    "attribute" = NULL,
                    "metadata" = list("contact" = email_format("helpdesk@obis.org"), 
                                      "url" = lang_list("<https://obis.org/>"),
                                      "accessedOnStr" = list("en" ="December 17 2021 using robis", 
                                                             "fr" = "17 decmbre 2021 par robis") ,
                                      "accessDate" = as.Date("2021-12-17"),
                                      "searchYears" = "2010-2021",
                                      "securityLevel" = noneList,
                                      "qualityTier" = variableQuality,
                                      "constraints" = noneList
                    )
)
save(obisFish_rr, file = file.path(localFileSavePath, "Open/obisFish_rr.RData"))
