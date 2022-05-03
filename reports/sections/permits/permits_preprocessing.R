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

# Determine file path for permit data
xl_data = file.path(fileLoadPath, "NaturalResources/Species/Permits/SARAdata_withCoordinates.xlsx")

# Get the sheet names from the spreadsheet (there is one sheet per year, from 2010 to 2020)
sheets = excel_sheets(path = xl_data)

# Read in the data from each sheet in the Excel file. Each sheet will be its own list
list_all = lapply(sheets, function (x) read_excel (xl_data, sheet = x))

# Combine each list into a single dataframe
permits = rbind.fill(list_all)

# Get scientific name. The name will be listed within brackets.
species_brackets = str_extract_all(permits$Species, "\\([^()]+\\)")

# Remove the brackets and add this as a new column to the permits data frame
permits$scientificName = substring(species_brackets, 2, nchar(species_brackets)-1)

# Remove sea turtle data (we aren't including this for now)
permits = permits[!(permits$scientificName=="Dermochelys coriacea"),]

# Then combine with the MAR species spreadsheet to get other relevant info (e.g., SARA/COSEWIC statuses)
comboPermits = full_join(permits, listed_species, by=c("scientificName" = "Scientific Name"))

comboPermits = dplyr::select(comboPermits, Year, LatDD, LongDD, "Scientific Name")





# Select required columns
permits = dplyr::select(permits, Year, LatDD, LongDD, Species)

#permits = dplyr::rename(wsdb,c("Scientific Name" = "scientificName", 
                               "COMMONNAME"))







# permits <- dplyr::select(wsdb, COMMONNAME, SCIENTIFICNAME, YEAR, LATITUDE, LONGITUDE)
# permits <- wsdb %>% dplyr::filter(YEAR >= rrMinYear)
# permits <- dplyr::rename(wsdb,c("Scientific Name" = "SCIENTIFICNAME",
#                              "CNAME"= COMMONNAME))
# permits <- merge(wsdb, cetLegend, by='Scientific Name')
# permits <- dplyr::select(wsdb, CNAME, 'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
# permits_sf <- sf::st_as_sf(wsdb, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
# permits_sf <- sf::st_crop(wsdb_sf, region_sf)

permits_rr <- list("title" = "Section 73 Permits",
                #"data_sf" = permits_sf,
                "attribute" = "Legend",
                "metadata" = list("contact" = "<Sean.Butler@dfo-mpo.gc.ca>", 
                                  "url" = lang_list("<https://www.canada.ca/en/environment-climate-change/services/species-risk-public-registry/policies-guidelines/permitting-under-section-73.html>"),
                                  "accessedOnStr" = list("en" ="April 26, 2022 by Charlotte Smith", "fr" = "26 avril 2022 par Charlotte Smith") ,
                                  "accessDate" = as.Date("2022-04-26"),
                                  "searchYears" = paste(rrMinYear, "-2020", sep=""),
                                  "securityLevel" = noneList,
                                  "qualityTier" = mediumQuality,
                                  "constraints" = internalUse
                )
)
save(permits_rr, file = file.path(localFileSavePath, "Secure/permits_rr.RData"))
