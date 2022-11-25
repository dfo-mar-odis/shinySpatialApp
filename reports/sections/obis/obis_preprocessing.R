source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
library(robis)
source(here::here("config.R"))
library(robis)
library(rgbif)

loadResult <- load_rdata(c("CommonData", "obis_rr"), regionStr)

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

# add dates to citations:
citDate <- as.character(Sys.Date())
# Vertical bar: | in gsub strings gets treated as an OR clause:
datasetNS$citation <- gsub("INSERT DATE|yyyy-mm-dd|Access date", citDate, datasetNS$citation)

##############################################################################################################################
## Prepare and combine data into appropriate format

# Replace NA geometries with WKT:
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

names(obis_df)[names(obis_df) == 'url'] <- 'URL'
names(obis_df)[names(obis_df) == 'citation'] <- 'Citation'

# convert to sf
obis_sf <- sf::st_as_sf(obis_df, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# split off rr_otherspecies are from common data:
obis_sf <- dplyr::rename(obis_sf, "Scientific Name"="scientificName")
obis_sf <- subset(obis_sf, !(obis_sf$`Scientific Name` %in% rr_otherSpecies$Scientific_Name))

obis_rr <- list("title" = "Ocean Biodiversity Information System (OBIS)",
                "data_sf" = obis_sf,
                "attribute" = "NONE",
                "metadata" = list("contact" = email_format("helpdesk@obis.org"), 
                                  "url" = lang_list("<https://obis.org/>"),
                                  "accessedOnStr" = list("en" ="April 28 2022 using robis", 
                                                        "fr" = "28 avril 2022 par robis") ,
                                  "accessDate" = as.Date("2022-04-28"),
                                  "searchYears" = "2010-2021",
                                  "securityLevel" = noneList,
                                  "qualityTier" = variableQuality,
                                  "constraints" = noneList,
                                  "pipelinePath" = paste0(githubRepo, "reports/sections/obis/obis_preprocessing.R")
                                  
                                  
                )
)
save(obis_rr, file = file.path(localFileSavePath, "Open/obis_rr.RData"))

