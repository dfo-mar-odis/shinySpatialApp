source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))
library(readxl)
library(plyr)
library(tibble)
library(stringr)
library(dplyr)

loadResult = load_rdata(c("CommonData", "permits_rr"), regionStr)

#------------------permits--------------------
# SARA Section 73 Permits (permits)

# Determine file path for permit data (created by Charlotte Smith)
xl_data = file.path(fileLoadPath, "NaturalResources/Species/Permits/SARAdata_withCoordinates.xlsx")

# Get the sheet names from the spreadsheet (there is one sheet per year, from 2010 to 2020)
sheets = excel_sheets(path = xl_data)

# Read in the data from each sheet in the Excel file. Each sheet will be its own list
list_all = lapply(sheets, function (x) read_excel (xl_data, sheet = x))

# Combine each list into a single dataframe
perm_df = rbind.fill(list_all)

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
                   # A few entries had scientific names between commas instead of brackets. Remove these too
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

# Select the columns that will actually be used in the report
permits = dplyr::select(comboPermits, LatDD, LongDD, scientificName, `Common Name`, `COSEWIC status`,
                             `SARA status`)

# Convert to sf object
permits_sf = sf::st_as_sf(permits, coords = c("LatDD", "LongDD"), crs = 4326)
# Crop data to the region
permits_sf = sf::st_crop(permits_sf, region_sf)

# Store data as RR object and set metadata
permits_rr = list("title" = "Section 73 Permits", 
                  "data_sf" = permits_sf,
                  "attribute" = "Legend",
                  "metadata" = list("contact" = "<Sean.Butler@dfo-mpo.gc.ca>",
                                  "url" = lang_list("<https://www.canada.ca/en/environment-climate-change/services/species-risk-public-registry/policies-guidelines/permitting-under-section-73.html>"),
                                  "accessedOnStr" = list("en" ="April 26, 2022 by Charlotte Smith", "fr" = "26 avril 2022 par Charlotte Smith") ,
                                  "accessDate" = as.Date("2022-04-26"),
                                  "searchYears" = paste(rrMinYear, "-2020", sep=""),
                                  "securityLevel" = noneList,
                                  "qualityTier" = mediumQuality,
                                  "constraints" = internalUse))

# Save the data. Do this locally and on the IN folder
save(permits_rr, file = file.path(localFileSavePath, "Secure/permits_rr.RData"))
