source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))

library(rgbif)

loadResult <- load_rdata(c("CommonData", "gbif_rr"), regionStr)

print("-------------------GBIF-------------------")
##############################################################################################################################
# Downloading Global Biodiversity Information Facility (GBIF) data for the Reproducible Report: DRAFT
# Created by Stephen Finnis, Quentin Stoyel, and Catalina Gomez in December 2021
# Will obtain records for fish and invertebrate species, and cetaceans listed by the Species at Risk Act and/or
# assessed by the Committee on the Status of Endangered Wildlife in Canada
##############################################################################################################################
if (globalControlEnv$updateGeoms) {
  
  searchYears <- paste("2010,", as.integer(format(Sys.Date(), "%Y")))
  
  # Get species occurrence data
  regionWKT <- sf::st_as_text(region_sf$geometry)
  rawData = occ_data(scientificName = listed_species$`Scientific Name`, 
                     hasCoordinate=T, geometry = regionWKT,
                       year = searchYears)
  
  # Convert this information to a data table
  gbif_df = data.table::rbindlist(lapply(rawData, function(z) z$data), fill=T, 
                                  use.names=T)
  
  ############################################################################################################################################################################################################
  ## Get dataset information: citations, title (dataset name) and URLs
  # Some of this information looks like it's included in the occurrence data table above, but it is incorrect
  
  # Extract only unique datasetKeys (these are needed for generating citation information)
  # (Running a for loop for all the dataset keys in the data table is very slow)
  dsKey = unique(gbif_df$datasetKey)
  
  # Create a dataframe to store all the extracted information
  dsInfo_df = as.data.frame(dsKey)
  
  # Get citation information for each record
  # not sure if this is better than the for loops....
  gbifCitations <- lapply(dsKey ,gbif_citation)
  # convert citation info into a useful dataframe
  citation_df <- as.data.frame(do.call(rbind, gbifCitations))
  citationsOnly_df <- as.data.frame(do.call(rbind, citation_df$citation))
  
  dsInfo_df$citation <- unlist(citationsOnly_df$citation)
  
  # add dates to citations:
  citDate <- as.character(Sys.Date())
  dsInfo_df$citation <- gsub("yyyy-mm-dd", citDate, dsInfo_df$citation)
  
  dsInfo_df$title <- unlist(citationsOnly_df$title)
  
  # Get URLs for each dataset
  # Within the citation, extract https://doi.org and everything after until it gets to a space 
  dsInfo_df$url = dsInfo_df$citation %>% 
    stringr::str_extract(., "https://doi.org/.*") %>% # extract the DOI and everything after it
    stringr::str_split(" ") %>% # split string on whitespace 
    sapply(.,"[[",1) # grab first element in the list, which will always be the doi
  
  
  # Combine the occurrence and dataset dataframes based on the dataset keys
  gbif_df = full_join(gbif_df, dsInfo_df, by=c("datasetKey"="dsKey"))
  
  # Then combine with the MAR species spreadsheet to get other relevant info (e.g., SARA/COSEWIC schedules, and Schedule status)
  gbif_df = full_join(gbif_df, listed_species, by=c("species"="Scientific Name"))
  names(gbif_df)[names(gbif_df) == "species"] <- "Scientific Name"
  
  # Remove records from datasets that are already included in the report
  gbif_df = subset(gbif_df, title!="Maritimes Summer Research Vessel Surveys" & 
                     title!= "Maritimes Spring Research Vessel Surveys"& 
                     title!= "Maritimes 4VSW Research Vessel Surveys" & 
                     title!= "DFO Maritimes Region Cetacean Sightings"& # aka Whale Sightings Database
                     datasetKey!= "NA") # these represent species that didn't have any GBIF occurrences but got added when joined with the MAR species spreadsheet
  
  # Select the required columns. We may decide later that more are useful
  gbif_df = dplyr::select(gbif_df, "Scientific Name", "Common Name", COSEWIC.population, year,
                          url, citation, title, "COSEWIC status", SARA.schedule, COMMONNAME,
                          basisOfRecord, eventDate, coordinateUncertaintyInMeters,
                          decimalLatitude, decimalLongitude)
  
  names(gbif_df)[names(gbif_df) == 'url'] <- 'URL'
  names(gbif_df)[names(gbif_df) == 'citation'] <- 'Citation'
  
  # Separate out the iNaturalist records and the Machine Observations records for making tables/figures
  machineobs = subset(gbif_df, basisOfRecord == "MACHINE_OBSERVATION")
  everythingelse = subset(gbif_df, title != "iNaturalist Research-grade Observations" & # exclude iNaturalist data
                            basisOfRecord != "MACHINE_OBSERVATION") # exclude machine observation records
  
  gbif_sf <- sf::st_as_sf(gbif_df, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
  
  # split off rr_otherspecies are from common data
  gbif_sf <- subset(gbif_sf, !(gbif_sf$`Scientific Name` %in% rr_otherSpecies$Scientific_Name))
} else {
  gbif_sf <- gbif_rr$data_sf
}

gbif_rr <- list("title" = "Global Biodiversity Information Facility (GBIF)",
                "data_sf" = gbif_sf,
                "attribute" = "NONE",
                "metadata" = read_google_metadata("gbif_rr")
)
save(gbif_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/gbif_rr.RData"))

# Still need to Remove cetaceans that are >300m from shore (not for machine observation records)
