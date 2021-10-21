
source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))


region_sf <- st_read(here::here("app/studyAreaTest/geoms_slc_MarBioRegion.geojson"))
setwd(here::here("app/data/MAR"))
setwd("\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data")

loadResult <- load_rdata(c("conservationSites_rr", "wsdb_rr", "narwc_rr" ),  "MAR")


highQuality <- list("en" = "High", "fr" = "Élevée")
lowQuality <- list("en" = "Low", "fr" = "Faible")
noneList <- list("en" = "None", "fr"= "Aucun")
internalUse <- list("en" = "DFO INTERNAL USE ONLY", "fr" = "DFO INTERNAL USE ONLY")

# -------------Marine Protected Areas (mpa)---------------------
conservationSites_raw <- st_read("../Data/Management/MPAN-Draft/MPAN_DraftDesign_Maritimes/MPAN_DraftDesign_Maritimes.shp", stringsAsFactors = FALSE)
conservationSites_sf <- st_transform(conservationSites_raw, crs = 4326)
conservationSites_sf <- st_make_valid(conservationSites_sf)
conservationSites_sf$Legend <- as.factor(conservationSites_sf$STATUS)
levels(conservationSites_sf$Legend) <- list("Existing Network Site"="Existing", "Other Network Sites"="Proposed", "Area of Interest for Oceans Act MPA"="Proposed AOI", "Proposed Conservation Area Under the Fisheries Act"="Proposed SBA")
conservationSites_sf$Legend <- as.character(conservationSites_sf$Legend)
conservationSites_sf[conservationSites_sf$Id == 18, "Legend" ] <- "Bras d'Or Lake -sites to be determined"

conservationSites_rr <- list("title" = "Draft Conservation Network Design",
                             "contact" = "Marty King ([Marty.King@dfo-mpo.gc.ca](mailto:Marty.King@dfo-mpo.gc.ca){.email})", 
                             "accessedOnStr" = list("en" ="October 2021", "fr" = "octobre, 2021") ,
                             "accessDate" = as.Date("2021-10-01"),
                             "data_sf" = conservationSites_sf,
                             "attribute" = "Legend",
                             "securityLevel" = noneList,
                             "qualityTier" = highQuality,
                             "constraints" = internalUse
)
save(conservationSites_rr, file = "./Secure/conservationSites_rr.RData")


#------------------CETACEANS-------------------

# Cetacean legend file
Legend <- read.csv("../Data/NaturalResources/Species/Cetaceans/CetaceanLegend.csv", stringsAsFactors = FALSE)
Legend <- dplyr::rename(Legend,c("Scientific Name" = "Scientific_Name"))

#------------------WSDB--------------------

# Cetacean point data  #########################
# Whale Sightings Database (wsdb)
wsdb <- read.csv("../Data/NaturalResources/Species/Cetaceans/WSDB/MarWSDB_20210407.csv", stringsAsFactors = FALSE)
wsdb <- dplyr::select(wsdb, COMMONNAME, SCIENTIFICNAME, YEAR, LATITUDE, LONGITUDE)
wsdb <- wsdb %>% dplyr::filter(YEAR >= 2010)
wsdb <- dplyr::rename(wsdb,c("Scientific Name" = "SCIENTIFICNAME",
                             "CNAME"= COMMONNAME))
wsdb <- merge(wsdb, Legend, by='Scientific Name')
wsdb <- dplyr::select(wsdb, CNAME, 'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
wsdb_sf <- st_as_sf(wsdb, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
wsdb_sf <- sf::st_crop(wsdb_sf, region_sf)

wsdb_rr <- list("title" = "Whale Sightings Database",
                "contact" = "<XMARWhaleSightings@dfo-mpo.gc.ca>", 
                "url" = lang_list("<http://www.inter.dfo-mpo.gc.ca/Maritimes/SABS/popec/sara/Database>"),
                "accessedOnStr" = list("en" ="October 27, 2020 by Shelley Lang", "fr" = "27 octobre 2020 par Shelley Lang  ") ,
                "accessDate" = as.Date("2020-10-27"),
                "data_sf" = wsdb_sf,
                "attribute" = "Legend",
                "securityLevel" = noneList,
                "qualityTier" = lowQuality,
                "constraints" = internalUse
)
save(wsdb_rr, file = "./Secure/wsdb_rr.RData")


# ----------------WHITEHEAD--------------
# Whitehead lab
whitehead <- read.csv("../Data/NaturalResources/Species/Cetaceans/Whitehead_Lab/whitehead_lab.csv", stringsAsFactors = FALSE)
whitehead$YEAR <- lubridate::year(whitehead$Date)
whitehead <- whitehead %>% dplyr::filter(YEAR >= 2010)
whitehead <- whitehead %>% rename("Scientific Name"= species.name)
whitehead <- merge(whitehead, Legend, by='Scientific Name')
whitehead <- dplyr::select(whitehead, 'Scientific Name', YEAR, Legend, Lat, Long)
# correct the longitude values to be negative
whitehead$Long <- -1 * whitehead$Long
whitehead_sf <- st_as_sf(whitehead, coords = c("Long", "Lat"), crs = 4326)
whitehead_sf <- sf::st_crop(whitehead_sf, region_sf)

whitehead_rr <- list("title" = "Whitehead lab (Dalhousie University)",
                "contact" = "<XMARWhaleSightings@dfo-mpo.gc.ca>", 
                "url" = lang_list("<https://whiteheadlab.weebly.com/contact.html>"),
                "accessedOnStr" = list("en" ="January 12 2021  by Laura Feyrer", "fr" = "12 janvier 2021 par Laura Feyrer  ") ,
                "accessDate" = as.Date("2021-01-12"),
                "data_sf" = whitehead_sf,
                "attribute" = "Legend",
                "securityLevel" = noneList,
                "qualityTier" = highQuality,
                "constraints" = internalUse
)
save(whitehead_rr, file = "./Secure/whitehead_rr.RData")



# ----------------------NARWC-------------------------
# North Atlantic Right Whale Consortium (narwc)
narwc <- read.csv("../Data/NaturalResources/Species/Cetaceans/NARWC/NARWC_09-18-2020.csv", stringsAsFactors = FALSE)
narwcspecies <-  read.csv("../Data/NaturalResources/Species/Cetaceans/NARWC/NARWCSpeciesNames.csv", stringsAsFactors = FALSE)
narwcspecies <- narwcspecies %>% rename("Scientific Name"= ScientificName)
narwc <- merge(narwc, narwcspecies, by='SPECNAME')
narwc <- narwc %>% dplyr::filter(YEAR >= 2010)
narwc <- merge(narwc, Legend, by = 'Scientific Name')
narwc <- dplyr::select(narwc, 'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
narwc_sf <- st_as_sf(narwc, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
narwc_sf <- sf::st_crop(narwc_sf, region_sf)


narwc_rr <- list("title" = "North Atlantic Right Whale consortium",
                 "contact" = "<hpettis@neaq.org>", 
                 "url" = lang_list("<https://www.narwc.org/sightings-database.html>"),
                 "accessedOnStr" = list("en" ="September 18 2020", "fr" = "18 septembre 2020") ,
                 "accessDate" = as.Date("2020-09-18"),
                 "data_sf" = narwc_sf,
                 "attribute" = "Legend",
                 "securityLevel" = noneList,
                 "qualityTier" = highQuality,
                 "constraints" = internalUse
)
save(narwc_rr, file = "./Secure/narwc_rr.RData")


