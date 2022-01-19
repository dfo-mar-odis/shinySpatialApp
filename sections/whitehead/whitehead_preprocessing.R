source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

source(here::here("config.R"))
fileSavePath <- here::here("app/data/MAR")


loadResult <- load_rdata(c("CommonData", "whitehead_rr"), regionStr)

# ----------------WHITEHEAD--------------
# Whitehead lab
whitehead <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/Whitehead_Lab/whitehead_lab.csv"), stringsAsFactors = FALSE)
whitehead$YEAR <- lubridate::year(whitehead$Date)
whitehead <- whitehead %>% dplyr::filter(YEAR >= rrMinYear)
whitehead <- whitehead %>% rename("Scientific Name"= species.name)
whitehead <- merge(whitehead, cetLegend, by='Scientific Name')
whitehead <- dplyr::select(whitehead, 'Scientific Name', YEAR, Legend, Lat, Long)
# correct the longitude values to be negative
whitehead$Long <- -1 * whitehead$Long
whitehead_sf <- st_as_sf(whitehead, coords = c("Long", "Lat"), crs = 4326)
whitehead_sf <- sf::st_crop(whitehead_sf, region_sf)

whitehead_rr <- list("title" = "Whitehead lab (Dalhousie University)",
                     "data_sf" = whitehead_sf,
                     "attribute" = "Legend",
                     "metadata" = list("contact" = "<XMARWhaleSightings@dfo-mpo.gc.ca>", 
                                       "url" = lang_list("<https://whiteheadlab.weebly.com/contact.html>"),
                                       "accessedOnStr" = list("en" ="January 12 2021  by Laura Feyrer", "fr" = "12 janvier 2021 par Laura Feyrer  ") ,
                                       "accessDate" = as.Date("2021-01-12"),
                                       "searchYears" = paste(rrMinYear, "-2019", sep=""),
                                       "securityLevel" = noneList,
                                       "qualityTier" = highQuality,
                                       "constraints" = internalUse
                     )
)
save(whitehead_rr, file = file.path(fileSavePath, "Secure/whitehead_rr.RData"))
