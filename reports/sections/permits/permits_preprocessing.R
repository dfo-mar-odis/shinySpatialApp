source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))
library(plyr)
library(tibble)
library(stringr)
library(dplyr)

loadResult = load_rdata(c("CommonData", "permits_rr"), regionStr)

print("------------------permits--------------------")

if (globalControlEnv$updateGeoms) {
    
  # SARA Section 73 Permits (permits)
  egisUrl <- "https://gisd.dfo-mpo.gc.ca/arcgis/rest/services/Maritimes_EM/SAR_Occurrences_S73Permits__2010_2020/MapServer/"
  egisLayer <- paste0(egisUrl, "1/")
  
  infoUrl <-"https://gisd.dfo-mpo.gc.ca/arcgis/rest/info"
  tokenUrl <- "https://gisd.dfo-mpo.gc.ca/portal/sharing/rest/generateToken"
  
  token <- get_token(tokenUrl, egisUrl)
  
  rawPermits <- esri2sf::esri2sf(egisLayer, token = token, progress = TRUE)
  perm_df <- sf::st_drop_geometry(rawPermits) 
  
  # Get scientific name. The name will be listed within brackets
  species_brackets = str_extract_all(perm_df$Species, "\\([^()]+\\)")
  
  # Remove the brackets and add this as a new column to the perm_df data frame
  perm_df$scientificName = substring(species_brackets, 2, nchar(species_brackets)-1)
  
  # There are some species that need to be removed/edited in the permit spreadsheets.
  # I noticed these issues when joining with the MAR spreadsheet below.
  perm_df = subset(perm_df, 
                   # Remove common minke whale: not COSEWIC-listed or Schedule 1
                   scientificName != "Balaena rostrata" & 
                     # Remove ones with an extra space in the name
                     scientificName != " Balaena rostrata" &
                     # Remove leatherback sea turtles (we aren't including these for now)
                     scientificName != "Dermochelys coriacea" & 
                     # Remove ones with an extra space in the name
                     scientificName != "Dermochelys coriacea " & 
                     # A few humpback whale entries had scientific names between commas instead of brackets. Remove these too
                     scientificName != "haracter(0" &
                     # Remove (NW Atlantic) humpback whales: not COSEWIC-listed or Schedule 1
                     scientificName != "Megaptera novaeangliae")
                     
  # Remove a space (typo) from a record for blue whales
  perm_df["scientificName"][perm_df["scientificName"] == " Balaenoptera musculus"] = "Balaenoptera musculus"
  # Remove the "vomerina". That's the Pacific population. I think it's a mistake. 
  perm_df["scientificName"][perm_df["scientificName"] == "Phocoena phocoena vomerina"] = "Phocoena phocoena"
  
  # Then combine with the MAR species spreadsheet to get other relevant info based on a common field
  comboPermits = full_join(perm_df, listed_species, by=c("scientificName" = "Scientific Name"))
  # Remove species information that got added from the full_join above. These are species not observed in Charlotte's spreadsheet
  # Species not in Charlotte's spreadsheet will have NA values in a variety of fields.
  comboPermits = subset(comboPermits, 
                        LongDD != "NA" &
                        LatDD != "NA") # there were also some entries with  missing latitudes!! Remove these.
  
  # Select the columns that will actually be used in the report. Plus a few other columns just in case.
  permits = dplyr::select(comboPermits, LatDD, LongDD, scientificName, `Common Name`, `COSEWIC status`,
                               `SARA status`)
  
  # Convert to sf object. CRS 4326 is WGS84
  permits_sf = sf::st_as_sf(permits, coords = c("LongDD", "LatDD"), crs = 4326)
  
  # set scientific name column:
  names(permits_sf)[names(permits_sf) == 'scientificName'] <- 'Scientific Name'
} else {
  permits_sf <- permits_rr$data_sf
}
# Store data as RR object and set metadata
permits_rr = list("title" = "Section 73 Permits", 
                  "data_sf" = permits_sf,
                  "attribute" = "Legend",
                  "metadata" = read_google_metadata("permits_rr")
                  )

# Save the data. Do this locally and on the IN folder
save(permits_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Secure/permits_rr.RData"))
