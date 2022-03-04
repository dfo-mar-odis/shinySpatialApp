source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "ocearch_rr"), regionStr)


# --------------------OCEARCH---------------------
# LOAD OCEARCH DATA #############
ocearchDatafile <- file.path(fileLoadPath, "NaturalResources/Species/Sharks/OCEARCH/OCEARCH_08-27-2021.csv")
lines <- readLines(ocearchDatafile)
lines <- gsub('(^"|"$)', "", lines)
ocearch <- read.csv(textConnection(lines), quote = '""')
ocearch <- dplyr::select(ocearch, c("Date", "long", "lat", "ID"))
ocearch_sf <- st_as_sf(ocearch, coords = c("long", "lat"), crs = 4326)
ocearch_sf <- sf::st_crop(ocearch_sf, region_sf)

ocearch_rr <- list("title" = "OCEARCH Shark Tracker",
                   "data_sf" = ocearch_sf,
                   "attribute" = "NONE",
                   "metadata" = list("contact" = paste("Bryan Franks (", email_format("bfranks@ju.edu"),  ") via Sean Butler (", email_format("sean.butler@dfo-mpo.gc.ca"), ")", sep=""), 
                                     "url" = lang_list("<https://www.ocearch.org/tracker/>"),
                                     "accessedOnStr" = list("en" ="July 22 2021 by Sean Butler", "fr" = "22 juillet 2021 par Sean Butler") ,
                                     "accessDate" = as.Date("2021-07-22"),
                                     "searchYears" = "2013-2020",
                                     "securityLevel" = noneList,
                                     "qualityTier" = highQuality,
                                     "constraints" = internalUse
                   )
)
save(ocearch_rr, file = file.path(localFileSavePath, "Secure/ocearch_rr.RData"))
