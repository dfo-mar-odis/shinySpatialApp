source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "permits_rr"), regionStr)

#------------------permits--------------------
# SARA Section 73 Permits (permits)
# permits <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/WSDB/MarWSDB_20210407.csv"), stringsAsFactors = FALSE)
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
                                  "searchYears" = paste(rrMinYear, "-2022", sep=""),
                                  "securityLevel" = noneList,
                                  "qualityTier" = mediumQuality,
                                  "constraints" = internalUse
                )
)
save(permits_rr, file = file.path(localFileSavePath, "Secure/permits_rr.RData"))
