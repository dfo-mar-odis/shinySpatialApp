source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))


region_sf <- st_read(here::here("app/studyAreaTest/geoms_slc_MarBioRegion.geojson"))
fileSavePath <- here::here("app/data/MAR")
fileLoadPath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data"

loadResult <- load_rdata(c("CommonData", "conservationSites_rr", "wsdb_rr", "whitehead_rr",
                           "narwc_rr", "leatherback_rr", "ocearch_rr"),  "MAR")


highQuality <- list("en" = "High", "fr" = "Élevée")
mediumQuality <- list("en" = "Medium", "fr" = "Moyenne")
lowQuality <- list("en" = "Low", "fr" = "Faible")
noneList <- list("en" = "None", "fr"= "Aucun")
protectedBList <- list("en" = "Protected B", "fr" = "Protégé B")
internalUse <- list("en" = "DFO INTERNAL USE ONLY", "fr" = "DFO INTERNAL USE ONLY")

# -------------Marine Protected Areas (mpa)---------------------
conservationSites_raw <- st_read(file.path(fileLoadPath, "Management/MPAN-Draft/MPAN_DraftDesign_Maritimes/MPAN_DraftDesign_Maritimes.shp"), stringsAsFactors = FALSE)
conservationSites_sf <- st_transform(conservationSites_raw, crs = 4326)
conservationSites_sf <- st_make_valid(conservationSites_sf)
conservationSites_sf$Legend <- as.factor(conservationSites_sf$STATUS)
levels(conservationSites_sf$Legend) <- list("Existing Network Site"="Existing", "Other Network Sites"="Proposed", "Area of Interest for Oceans Act MPA"="Proposed AOI", "Proposed Conservation Area Under the Fisheries Act"="Proposed SBA")
conservationSites_sf$Legend <- as.character(conservationSites_sf$Legend)
conservationSites_sf[conservationSites_sf$Id == 18, "Legend" ] <- "Bras d'Or Lake -sites to be determined"

conservationSites_rr <- list("title" = "Draft Conservation Network Design",
                             "data_sf" = conservationSites_sf,
                             "attribute" = "Legend",
                             "metadata" = list("contact" = "Marty King ([Marty.King@dfo-mpo.gc.ca](mailto:Marty.King@dfo-mpo.gc.ca){.email})", 
                                               "accessedOnStr" = list("en" ="October 2021", "fr" = "octobre, 2021") ,
                                               "accessDate" = as.Date("2021-10-01"),
                                               "securityLevel" = noneList,
                                               "qualityTier" = highQuality,
                                               "constraints" = internalUse
                             )
)

save(conservationSites_rr, file = file.path(fileSavePath, "Secure/conservationSites_rr.RData"))

#------------------CETACEANS-------------------

#------------------WSDB--------------------
# Whale Sightings Database (wsdb)
wsdb <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/WSDB/MarWSDB_20210407.csv"), stringsAsFactors = FALSE)
wsdb <- dplyr::select(wsdb, COMMONNAME, SCIENTIFICNAME, YEAR, LATITUDE, LONGITUDE)
wsdb <- wsdb %>% dplyr::filter(YEAR >= 2010)
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
                                  "searchYears" = "2010-2020",
                                  "securityLevel" = noneList,
                                  "qualityTier" = lowQuality,
                                  "constraints" = internalUse
                )
)
save(wsdb_rr, file = file.path(fileSavePath, "Secure/wsdb_rr.RData"))


# ----------------WHITEHEAD--------------
# Whitehead lab
whitehead <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/Whitehead_Lab/whitehead_lab.csv"), stringsAsFactors = FALSE)
whitehead$YEAR <- lubridate::year(whitehead$Date)
whitehead <- whitehead %>% dplyr::filter(YEAR >= 2010)
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
                                       "searchYears" = "2010-2019",
                                       "securityLevel" = noneList,
                                       "qualityTier" = highQuality,
                                       "constraints" = internalUse
                     )
)
save(whitehead_rr, file = file.path(fileSavePath, "Secure/whitehead_rr.RData"))

# ----------------------NARWC-------------------------
# North Atlantic Right Whale Consortium (narwc)
narwc <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/NARWC/NARWC_09-18-2020.csv"), stringsAsFactors = FALSE)
narwcspecies <-  read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/NARWC/NARWCSpeciesNames.csv"), stringsAsFactors = FALSE)
narwcspecies <- narwcspecies %>% rename("Scientific Name"= ScientificName)
narwc <- merge(narwc, narwcspecies, by='SPECNAME')
narwc <- narwc %>% dplyr::filter(YEAR >= 2010)
narwc <- merge(narwc, cetLegend, by = 'Scientific Name')
narwc <- dplyr::select(narwc, 'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
narwc_sf <- st_as_sf(narwc, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
narwc_sf <- sf::st_crop(narwc_sf, region_sf)

narwc_rr <- list("title" = "North Atlantic Right Whale consortium",
                 "data_sf" = narwc_sf,
                 "attribute" = "Legend",
                 "metadata" = list("contact" = "<hpettis@neaq.org>", 
                                   "url" = lang_list("<https://www.narwc.org/sightings-database.html>"),
                                   "accessedOnStr" = list("en" ="September 18 2020", "fr" = "18 septembre 2020") ,
                                   "accessDate" = as.Date("2020-09-18"),
                                   "searchYears" = "2010-2019",
                                   "securityLevel" = noneList,
                                   "qualityTier" = highQuality,
                                   "constraints" = internalUse
                                   )
)
save(narwc_rr, file = file.path(fileSavePath, "Secure/narwc_rr.RData"))


# ------------LEATHERBACKS--------------------

# Leatherback turtle habitat
leatherback_sf <- st_read(file.path(fileLoadPath, "NaturalResources/Species/SpeciesAtRisk/LeatherBackTurtleCriticalHabitat/LBT_CH_2013.shp"), stringsAsFactors = FALSE)
leatherback_sf <- st_make_valid(leatherback_sf)
leatherback_sf <- sf::st_crop(leatherback_sf, region_sf)


leatherback_rr <- list("title" = " Leatherback Sea Turtle draft critical habitat",
                       "data_sf" = leatherback_sf,
                       "attribute" = "NONE",
                       "metadata" = list("contact" = email_format("info@dfo-mpo.gc.ca"),
                                         "url" = lang_list("<https://www.narwc.org/sightings-database.html>"),
                                         "accessedOnStr" = list("en" ="February 25 2021", "fr" = "25 février 2021") ,
                                         "accessDate" = as.Date("2021-02-25"),
                                         "securityLevel" = noneList,
                                         "qualityTier" = highQuality,
                                         "constraints" = internalUse
                       )
)
save(leatherback_rr, file = file.path(fileSavePath, "Secure/leatherback_rr.RData"))


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
save(ocearch_rr, file = file.path(fileSavePath, "Secure/ocearch_rr.RData"))


# ---------------------ELECTROFISHING-----------------------------------

# Electrofishing
efSpecList <- data.frame("colNames" = c("AlosaUnidentified", "AmericanEel", "AtlanticSalmon", 
                                        "BlacknoseDace", "BrookTrout", "BrownTrout", 
                                        "ChubSpp", "CommonShiner", "CreekChub",  
                                        "GoldenShiner", "LakeChub", "Lamprey", "Mummichog", 
                                        "OtherCyprinids", "OtherSpp", "RainbowTrout",
                                        "SeaLamprey", "SlimySculpin", "SmallmouthBass", 
                                        "SticklebackSpp", "ThreespinedStickleback", "WhiteSucker"),
                 "Common.Names" = c("Unidentified Alosa", "American Eel", "Atlantic Salmon", 
                                    "Blacknose Dace", "Brook Trout", "Brown Trout", 
                                    "Chub Spp", "Common Shiner", "Creek Chub",  
                                    "Golden Shiner", "Lake Chub", "Lamprey", "Mummichog", 
                                    "Other Cyprinids", "Other Spp", "Rainbow Trout",
                                    "Sea Lamprey", "Slimy Sculpin", "Smallmouth Bass", 
                                    "Stickleback Spp", "Three spined Stickleback", "White Sucker"), 
                 "Scientific.Names" = c("Alosa", "Anguilla rostrata", "Salmo salar", 
                                        "Rhinichthys atratulus", "Salvelinus fontinalis", 
                                        "Salmo trutta", "Cyprinidae Spp",
                                        "Luxilus cornutus", "Semotilus atromaculatus",  
                                        "Notemigonus crysoleucas", "Couesius plumbeus", 
                                        "Petromyzontiformes", "Fundulus heteroclitus", 
                                        "Other Cyprinids", "Other Spp", "Oncorhynchus mykiss",
                                        "Petromyzon marinus", "Cottus cognatus",
                                        "Micropterus dolomieu", "Gasterosteidae Spp",
                                        "Gasterosteus aculeatus", "Catostomus commersonii"))


efSurvey_to_sf <- function(surveyData, efSpecList, region_sf){
  extraCols <- c("Year_", "DataSource", "Document")
  surveyLong <- tidyr::pivot_longer(surveyData, cols = any_of(efSpecList$colNames), names_to = "colNames", values_to = "count")
  surveyLong <- dplyr::filter(surveyLong, count > 0)
  surveyLong <- dplyr::filter(surveyLong, Year_ > 2008)
  surveyLong <- dplyr::select(surveyLong, all_of(c(extraCols, "Shape", "colNames")))
  surveyLong <- left_join(surveyLong, efSpecList, by = "colNames")
  survey_sf <- st_transform(st_as_sf(surveyLong), crs=4326)
  survey_sf <- st_crop(survey_sf, region_sf)
  survey_sf <- dplyr::select(survey_sf, !colNames)
  names(survey_sf ) <- c("Year", "DataSource", "Document", "Common Name", "Scientific Name", "geometry")
  st_geometry(survey_sf) <- "geometry"
  return(survey_sf)
}

efGdbPath <- file.path(fileLoadPath, "NaturalResources/Species/Electrofishing/EFishing_DFOScience.gdb")
st_layers(efGdbPath)

efSurvey2009_sf <- st_read(efGdbPath, layer = "EFish_ECB_2006_2007_BowlbyGibson2009" )
efSurvey2013_sf <- st_read(efGdbPath, layer = "EFish_SU_2000_2008_RPA_2013" )
efSurvey2018A_sf <- st_read(efGdbPath, layer = "EFish_IBoF_2014_nonLGB_Rivers_2018" )
efSurvey2018B_sf <- st_read(efGdbPath, layer = "EFish_IBoF_2013_Stewiacke_2018" )

ef_sf <- efSurvey_to_sf(efSurvey2009_sf, efSpecList, region_sf)
ef_sf <- rbind(ef_sf, efSurvey_to_sf(efSurvey2013_sf, efSpecList, region_sf))
ef_sf <- rbind(ef_sf, efSurvey_to_sf(efSurvey2018A_sf, efSpecList, region_sf))
ef_sf <- rbind(ef_sf, efSurvey_to_sf(efSurvey2018B_sf, efSpecList, region_sf))


ef_rr <- list("title" = "Electrofishing Data","data_sf" = ef_sf,
              "attribute" = "Year",
              "metadata" = list("contact" = email_format("Dustin.Raab@dfo-mpo.gc.ca"), 
                                "accessedOnStr" = list("en" ="October 18 2021 by Sean Butler", "fr" = "18 octobre 2021 par Sean Butler") ,
                                "accessDate" = as.Date("2021-10-18"),
                                "searchYears" = "2008-2014",
                                "securityLevel" = noneList,
                                "qualityTier" = highQuality,
                                "constraints" = internalUse
              )
                   
)
save(ef_rr, file = file.path(fileSavePath, "Secure/ef_rr.RData"))

