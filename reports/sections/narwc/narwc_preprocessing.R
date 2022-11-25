source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "narwc_rr"), regionStr)

# ----------------------NARWC-------------------------
# North Atlantic Right Whale Consortium (narwc)
narwc <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/NARWC/NARWC_09-18-2020.csv"), stringsAsFactors = FALSE)
narwcspecies <-  read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/NARWC/NARWCSpeciesNames.csv"), stringsAsFactors = FALSE)
narwcspecies <- narwcspecies %>% rename("Scientific Name"= ScientificName)
narwc <- merge(narwc, narwcspecies, by='SPECNAME')
narwc <- narwc %>% dplyr::filter(YEAR >= rrMinYear)
narwc <- merge(narwc, cetLegend, by = 'Scientific Name')
narwc <- dplyr::select(narwc, 'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
narwc_sf <- sf::st_as_sf(narwc, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
narwc_sf <- sf::st_crop(narwc_sf, region_sf)

narwc_rr <- list("title" = "North Atlantic Right Whale consortium",
                 "data_sf" = narwc_sf,
                 "attribute" = "Legend",
                 "metadata" = list("contact" = "<hpettis@neaq.org>", 
                                   "url" = lang_list("<https://www.narwc.org/sightings-database.html>"),
                                   "accessedOnStr" = list("en" ="September 18 2020", "fr" = "18 septembre 2020") ,
                                   "accessDate" = as.Date("2020-09-18"),
                                   "searchYears" = paste(rrMinYear, "-2019", sep=""),
                                   "securityLevel" = noneList,
                                   "qualityTier" = highQuality,
                                   "constraints" = internalUse,
                                   "pipelinePath" = paste0(githubRepo, "reports/sections/narwc/narwc_preprocessing.R")
                 )
)
save(narwc_rr, file = file.path(localFileSavePath, "Secure/narwc_rr.RData"))
