source(here::here("reports/R/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

fileSavePath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\RData\\data\\MAR"
fileSavePath <- here::here("app/data/MAR")


loadResult <- load_rdata(c("CommonData", "wsdb_rr"), regionStr)

#------------------WSDB--------------------
# Whale Sightings Database (wsdb)
wsdb <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/WSDB/MarWSDB_20210407.csv"), stringsAsFactors = FALSE)
wsdb <- dplyr::select(wsdb, COMMONNAME, SCIENTIFICNAME, YEAR, LATITUDE, LONGITUDE)
wsdb <- wsdb %>% dplyr::filter(YEAR >= rrMinYear)
wsdb <- dplyr::rename(wsdb,c("Scientific Name" = "SCIENTIFICNAME",
                             "CNAME"= COMMONNAME))
wsdb <- merge(wsdb, cetLegend, by='Scientific Name')
wsdb <- dplyr::select(wsdb, CNAME, 'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
wsdb_sf <- st_as_sf(wsdb, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
wsdb_sf <- sf::st_crop(wsdb_sf, region_sf)

wsdb_rr <- list("title" = "Whale Sightings Database",
                "data_sf" = wsdb_sf,
                "attribute" = "Legend",
                "metadata" = list("contact" = "<XMARWhaleSightings@dfo-mpo.gc.ca>", 
                                  "url" = lang_list("<http://www.inter.dfo-mpo.gc.ca/Maritimes/SABS/popec/sara/Database>"),
                                  "accessedOnStr" = list("en" ="October 27, 2020 by Shelley Lang", "fr" = "27 octobre 2020 par Shelley Lang  ") ,
                                  "accessDate" = as.Date("2020-10-27"),
                                  "searchYears" = paste(rrMinYear, "-2020", sep=""),
                                  "securityLevel" = noneList,
                                  "qualityTier" = lowQuality,
                                  "constraints" = internalUse
                )
)
save(wsdb_rr, file = file.path(fileSavePath, "Secure/wsdb_rr.RData"))