source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

fileSavePath <- here::here("app/data/MAR")
fileLoadPath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data"

region_sf <- st_read(here::here("app/studyAreaTest/geoms_slc_MarBioRegion.geojson"))
save(region_sf, file = file.path(fileSavePath,"Open/region_sf.RData"))

load(here::here("app/data/CommonData.RData"))
loadResult <- load_rdata(c("EBSA_rr", "crithab_rr", "sardist_rr", "nbw_rr", 
                           "bwhab_rr", "obisCet_rr", "finWhale_rr", 
                           "seiWhale_rr", "humpbackWhale_rr",
                           "harbourPorpoise_rr", "nbw_rr"),  "MAR")

highQuality <- list("en" = "High", "fr" = "Élevée")
mediumQuality <- list("en" = "Medium", "fr" = "Moyenne")
noneList <- list("en" = "None", "fr"= "Aucun")
internalUse <- list("en" = "DFO INTERNAL USE ONLY", "fr" = "DFO INTERNAL USE ONLY")


# ----------------COMMON DATA-------------
land10m_sf <- st_read(file.path(fileLoadPath, "Boundaries/Landmass/ne_10m_land_Clip.shp"), stringsAsFactors = FALSE)
#remove State and Province column from land10m
land10m_sf <- land10m_sf[-c(2)]

land50k_sf <- st_read(file.path(fileLoadPath, "Boundaries/Coast50k/Coastline50k_SHP/Land_AtlCanada_ESeaboardUS.shp"),
                      stringsAsFactors = FALSE)

# National boundaries (terrestrial and marine)
bounds_sf <- st_read(file.path(fileLoadPath, "Boundaries/AdminBoundaries/AdminBounds_SHP/Boundaries_Line.shp"), stringsAsFactors = FALSE)
bounds_sf <- dplyr::select(bounds_sf,SRC_DESC, geometry)
bounds_sf <- st_transform(bounds_sf, 4326) # Project to WGS84

# Table of SARA listed species
listed_species <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/MAR_listed_species.csv"),
                           stringsAsFactors = FALSE)
listed_species <- listed_species %>% rename("SARA status" = Schedule.status,
                                            "COSEWIC status" = COSEWIC.status,
                                            "Wild Species listing" = Wild_Species,
                                            "SCIENTIFICNAME" = Scientific_Name_upper,
                                            "COMMONNAME" = Common_Name_upper,
                                            "Scientific Name" = Scientific_Name,
                                            "Common Name" = Common_Name)
listed_species <- listed_species[!c(listed_species$`COSEWIC status` == "No Status" & listed_species$`SARA status` == "No Status"), ]
row.names(listed_species) <- NULL

# Cetacean legend file
cetLegend <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/CetaceanLegend.csv"), stringsAsFactors = FALSE)
cetLegend <- dplyr::rename(Legend,c("Scientific Name" = "Scientific_Name"))

save(land10m_sf, land50k_sf, bounds_sf, listed_species, cetLegend, file = file.path(fileSavePath, "CommonData.RData"))


# ----------------EBSA----------------- 
EBSApkgId <- "d2d6057f-d7c4-45d9-9fd9-0a58370577e0"
EBSAresId <- "ec990fd7-91b0-4dbb-a0f4-bb11070a84c1"

EBSAcheckDate <- get_check_date("EBSA_rr")

OpenEBSA_rr <- get_opendata_rr(EBSApkgId, EBSAresId, region_sf = region_sf,
                               checkDate = EBSAcheckDate)
if(!is.null(OpenEBSA_rr)) {
  EBSA_rr <- OpenEBSA_rr
  EBSA_rr$data_sf$Report_URL <- str_replace(EBSA_rr$data_sf$Report_URL,
                                            ".pdf", ".html")
  EBSA_rr$qualityTier <- highQuality
  EBSA_rr$contact <- email_format("carissa.philippe\\@dfo-mpo.gc.ca")
  save(EBSA_rr, file = file.path(fileSavePath, "Open/EBSA_rr.RData"))
}


# -----------SAR DIST--------------

sardistPkgId <- "e0fabad5-9379-4077-87b9-5705f28c490b"
sardistResId <- "d16d8405-dddf-491b-8faf-8cc7a9f3a537"

save_open_data(sardistPkgId, sardistResId, "sardist_rr", highQuality,
               fileSavePath, contactEmail = email_format("info\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, gdbLayer = "DFO_SARA_Dist_2021_FGP_EN")


# -----------CritHab-------------- # check table cols.
crithabPkgId <- "db177a8c-5d7d-49eb-8290-31e6a45d786c"
crithabResId <- "d6ce91ff-694f-4549-87ec-43f1a1bf5dca"
critHabLayer <- "DFO_SARA_CritHab_2021_FGP_EN"

critHabCheckDate <-  get_check_date("crithab_rr")

openCrithab_rr <- get_opendata_rr(crithabPkgId, crithabResId, 
                                  region_sf = region_sf,
                                  gdbLayer = critHabLayer, 
                                  checkDate = critHabCheckDate)
if(!is.null(openCrithab_rr)) {
  crithab_rr <- openCrithab_rr
  crithab_rr$qualityTier <- highQuality
  crithab_rr$contact <- email_format("info@dfo-mpo.gc.ca")
  crithab_rr$attribute <- "Common_Nam"
  crithab_rr$data_sf$Common_Nam <- crithab_rr$data_sf$Common_Name_EN
  save(crithab_rr, file = file.path(fileSavePath, "Open/crithab_rr.RData"))
}


# -----------SDM--------------
sdmPkgId <- "c094782e-0d6f-4cc0-b5a3-58908493a433"
sdmResId <- "16df15fb-367c-46e3-8ab7-be25315b9fbd"

save_open_data(sdmPkgId, sdmResId, "finWhale_rr", mediumQuality, fileSavePath,
               contactEmail = email_format("Hilary.Moors-Murphy\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, tifFile = "Fin_Whale.tif")

save_open_data(sdmPkgId, sdmResId, "seiWhale_rr", mediumQuality, fileSavePath,
               contactEmail = email_format("Hilary.Moors-Murphy\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, tifFile = "Sei_Whale.tif")

save_open_data(sdmPkgId, sdmResId, "humpbackWhale_rr", mediumQuality, fileSavePath,
               contactEmail = email_format("Hilary.Moors-Murphy\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, tifFile = "Humpback_Whale.tif")

save_open_data(sdmPkgId, sdmResId, "harbourPorpoise_rr", mediumQuality, fileSavePath,
               contactEmail = email_format("Hilary.Moors-Murphy\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, tifFile = "Harbour_Porpoise.tif")


# -----------NBW-------------- 
nbwPkgId <- "9fd7d004-970c-11eb-a2f3-1860247f53e3"
nbwResId <- "f69a7d34-7c18-485b-98d7-8d45b7f8a3ce"

save_open_data(nbwPkgId, nbwResId, "nbw_rr", highQuality, fileSavePath,
               contactEmail = email_format("MaritimesRAP.XMAR\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, gdbLayer = "NorthernBottlenoseWhale_InterCanyonHabitat")

# -----------BW hab-------------- 
bwhabPkgId <- "8fafd919-fcbe-43a3-a911-3d9461273441"
bwResId <- "3af8ad03-c0da-4cfa-940d-d757c0c24cb7"

bwCheckDate <- get_check_date("bwhab_rr")
openBwhab_rr <- get_opendata_rr(bwhabPkgId, bwResId, region_sf = region_sf,
                                checkDate = bwCheckDate)
if(!is.null(openBwhab_rr)) {
  bwhab_rr <- openBwhab_rr
  bwTemp_sf <- bwhab_rr$data_sf
  bwTemp_sf <- setNames(bwTemp_sf, replace(names(bwTemp_sf), names(bwTemp_sf) == 'activité', 'activite'))
  bwTemp_sf$activity[bwTemp_sf$activity == "foraging/Feeding"] <- "Foraging/Feeding"
  bwTemp_sf$activity[bwTemp_sf$activity == "Migrant"] <- "Migration"
  bwTemp_sf$months[bwTemp_sf$months == "all year"] <- "All year"
  bwTemp_sf$months[bwTemp_sf$months == "December to February/March to May"] <- "Dec-Feb/Mar-May"
  bwTemp_sf$months[bwTemp_sf$months == "December to February/June to August"] <- "Dec-Feb/Jun-Aug"
  bwTemp_sf$months[bwTemp_sf$months == "March to May/June to August"] <- "Mar-May/Jun-Aug"
  bwTemp_sf$Activity <- paste(bwTemp_sf$activity,"-", bwTemp_sf$months)
  bwhab_rr$data_sf <- bwTemp_sf
  
  bwhab_rr$contact <- email_format("gddaiss-dmsaisb\\@dfo-mpo.gc.ca")
  bwhab_rr$qualityTier <- highQuality
  bwhab_rr$attribute <- "Activity"
  
  save(bwhab_rr, file = file.path(fileSavePath, "Open/bwhab_rr.RData"))
}


#-------------------OBIS-------------------

# Legend file for displaying cetacean results in consistent colours
Legend <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/Cetaceans/CetaceanLegend.csv"), stringsAsFactors = FALSE)
Legend <- dplyr::rename(Legend,c("Scientific Name" = "Scientific_Name"))
cetacean_list <- c("BELUGA WHALE", "NORTH ATLANTIC RIGHT WHALE", "FIN WHALE", 
                   "NORTHERN BOTTLENOSE WHALE",
                   "HARBOUR PORPOISE", "KILLER WHALE", "BLUE WHALE", "SEI WHALE", "SOWERBY'S BEAKED WHALE")


other_species_list <- c("LOGGERHEAD SEA TURTLE", "ATLANTIC WALRUS", "HARBOUR SEAL LACS DES LOUPS MARINS SUBSPECIES", "LEATHERBACK SEA TURTLE")
listed_cetacean_species <- subset(listed_species, COMMONNAME %in% cetacean_list)
listed_other_species <- subset(listed_species, COMMONNAME %in% other_species_list)
listed_fish_invert_species <- listed_species[ ! listed_species$COMMONNAME %in% c(other_species_list,cetacean_list), ]

# Ocean Biogeographic Information System (OBIS)
obis <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/OBIS_GBIF_iNaturalist/OBIS_MAR_priority_records.csv"), stringsAsFactors = FALSE)
obis <- dplyr::select(obis, scientificName, decimalLatitude, decimalLongitude, year)
obis <- obis %>% transmute(obis, scientificName = str_to_sentence(scientificName))
obis <- obis %>% rename("Scientific Name" = scientificName, "YEAR" = year)
obis <- obis %>% dplyr::filter(YEAR >= 2010)

# OBIS fish and inverts
obisFish <- merge(obis, listed_fish_invert_species, by='Scientific Name')
obisFish <- dplyr::select(obisFish, "Scientific Name", YEAR, "Common Name", "COSEWIC status",
                           "SARA status", decimalLatitude, decimalLongitude)
obisFish_sf <- st_as_sf(obisFish, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
obisFish_sf <- sf::st_crop(obisFish_sf, region_sf)

obisFish_rr <- list("title" = "OBIS observations",
                   "contact" = email_format("helpdesk@obis.org"), 
                   "url" = lang_list("<https://obis.org/>"),
                   "accessedOnStr" = list("en" ="January 27 2021 by Gregory Puncher from OBIS", 
                                          "fr" = "27 janvier 2021 par Gregory Puncher du SIBO") ,
                   "accessDate" = as.Date("2021-01-27"),
                   "data_sf" = obisFish_sf,
                   "attribute" = "Legend",
                   "securityLevel" = noneList,
                   "qualityTier" = mediumQuality,
                   "constraints" = noneList
)
save(obisFish_rr, file = file.path(fileSavePath, "Open/obisFish_rr.RData"))

# OBIS cetaceans
obisCet <- merge(obis, Legend, by='Scientific Name')
obisCet <- dplyr::select(obisCet, "Scientific Name", YEAR, Legend,
                          decimalLatitude, decimalLongitude)
obisCet_sf <- st_as_sf(obisCet, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
obisCet_sf <- sf::st_crop(obisCet_sf, region_sf)


obisCet_rr <- list("title" = "OBIS observations (cetaceans)",
                   "contact" = email_format("helpdesk@obis.org"), 
                   "url" = lang_list("<https://obis.org/>"),
                   "accessedOnStr" = list("en" ="January 27 2021 by Gregory Puncher from OBIS", 
                                          "fr" = "27 janvier 2021 par Gregory Puncher du SIBO") ,
                   "accessDate" = as.Date("2021-01-27"),
                   "data_sf" = obisCet_sf,
                   "attribute" = "Legend",
                   "securityLevel" = noneList,
                   "qualityTier" = mediumQuality,
                   "constraints" = noneList
)
save(obisCet_rr, file = file.path(fileSavePath, "Open/obisCet_rr.RData"))


# ---------------------ROCKWEED------------------------------
# Rockweed
rockweed_sf <- st_read("../Data/NaturalResources/Species/Rockweed/MAR_rockweed_presence_validated.shp", stringsAsFactors = FALSE)
rockweed_sf <- st_transform(rockweed_sf, 4326) # Project to WGS84
rockweed_sf <- st_make_valid(rockweed_sf)
# set status column
rockweed_sf$status = ""
rockweed_sf$status[which(rockweed_sf$RWP==1)] = "Present"
rockweed_sf$status[which(rockweed_sf$RWP==2)] = "Likely Present"
rockweed_sf$status[which(rockweed_sf$RWP==5)] = "Unknown"
rockweed_sf$status[which(rockweed_sf$RWP==0)] = "Not Present"
rockweed_sf <- sf::st_crop(rockweed_sf, region_sf)


rockweed_rr <- list("title" = "Satellite-based Maps of Intertidal Vegetation and Rockweed presence polygons",
                    "contact" = email_format("Gordana.Lazin@dfo-mpo.gc.ca"), 
                    "accessedOnStr" = list("en" ="February 17 2021", "fr" = "17 février 2021") ,
                    "accessDate" = as.Date("2021-02-17"),
                    "data_sf" = rockweed_sf,
                    "attribute" = "status",
                    "securityLevel" = noneList,
                    "qualityTier" = mediumQuality,
                    "constraints" = internalUse
)
save(rockweed_rr, file = file.path(fileSavePath, "Open/rockweed_rr.RData"))


# -----------------------RV SURVEY---------------------
RVPkgId <- "8ddcaeea-b806-4958-a79f-ba9ab645f53b"

RV_rr <- get_opendata_rr(RVPkgId, NULL, region_sf = region_sf)
RV_rr$contact <- email_format("DFO.MAR-PED-Data-Request-Demande-de-donnes-DEP-MAR.MPO@dfo-mpo.gc.ca")
RV_rr$qualityTier <- highQuality

rvCsvList <- c("GSCAT.csv", "GSINF.csv", "GSSPECIES.csv")
minYear <- 2010
# 4VSW
fourVSWResId <- "ca308c48-2a87-4dcb-a4ba-90297b853635"
fourVSWDfs <- download_extract_res_files(fourVSWResId, rvCsvList)
fourVSW_sf <- RV_to_sf(fourVSWDfs[[1]], fourVSWDfs[[2]], fourVSWDfs[[3]], minYear)
# Spring
springResId <- "eef5180a-6a1b-4c85-aeb2-247afffb2077"
springDfs <- download_extract_res_files(springResId, rvCsvList)
spring_sf <- RV_to_sf(springDfs[[1]], springDfs[[2]], springDfs[[3]], minYear)
# Summer
summerResId <- "7f91a5ba-07dd-4494-b71d-89782cbb12bc"
summerDfs <- download_extract_res_files(summerResId, rvCsvList)
summer_sf <- RV_to_sf(summerDfs[[1]], summerDfs[[2]], summerDfs[[3]], minYear)
# Fall
fallResId <- "6abb48d0-384b-4c3e-8f13-56b74be919c4"
fallDfs <- download_extract_res_files(fallResId, rvCsvList)
fall_sf <- RV_to_sf(fallDfs[[1]], fallDfs[[2]], fallDfs[[3]], minYear)

rv_sf <- rbind(fourVSW_sf, spring_sf, summer_sf, fall_sf)
rv_sf <- sf::st_crop(rv_sf, region_sf)
RV_rr$data_sf <- rv_sf
save(RV_rr, file = file.path(fileSavePath, "Open/RV_rr.RData"))

