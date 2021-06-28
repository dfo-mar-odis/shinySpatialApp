library(sf)
library(raster)
library(dplyr)
library(stringr)
library(lubridate)

# change directory
source("SearchPEZ/code/fn_SurveyData_GetRV.R")

#### Arguments for RV survey data #################-
# for SelectRV_fn
SurveyPrefix <- c("4VSW", "FALL", "SPRING", "SUMMER")
File <- c("_2020_GSCAT.csv", "_2020_GSINF.csv", "_2020_GSSPECIES.csv")
minYear <- 2010


########################################################################-
# save open data as single .Rdata file

# landmass at two scales (1:10,000,000 and 1:50,000)

land10m_sf <- st_read("../Data/Boundaries/Landmass/ne_10m_land_Clip.shp", stringsAsFactors = FALSE)
#remove State and Province column from land10m
land10m_sf <- land10m_sf[-c(2)]

land50k_sf <- st_read("../Data/Boundaries/Coast50k/Coastline50k_SHP/Land_AtlCanada_ESeaboardUS.shp",
                      stringsAsFactors = FALSE)

# National boundaries (terrestrial and marine)
bounds_sf <- st_read("../Data/Boundaries/AdminBoundaries/AdminBounds_SHP/Boundaries_Line.shp", stringsAsFactors = FALSE)
bounds_sf <- dplyr::select(bounds_sf,SRC_DESC, geometry)
bounds_sf <- st_transform(bounds_sf, 4326) # Project to WGS84

# DFO bioregions layer (Maritime region)
bioregion_sf <- st_read("../Data/Boundaries/MaritimesRegionBound/MaritimesRegionPolygon_UpdatedSept2015.shp", stringsAsFactors = FALSE)
# reduce number of fields

# Rockweed
rockweed_sf <- st_read("../Data/NaturalResources/Species/Rockweed/MAR_rockweed_presence_validated.shp", stringsAsFactors = FALSE)
rockweed_sf <- st_transform(rockweed_sf, 4326) # Project to WGS84
rockweed_sf <- st_make_valid(rockweed_sf)
######## Habitat Information########
# rockweed_sf$RWP[which(rockweed_sf$RWP=="1")]= "Present"
# rockweed_sf$RWP[which(rockweed_sf$RWP=="2")]= "Likely Present"
# rockweed_sf$RWP[which(rockweed_sf$RWP=="5")]= "Unknown"

# Table of SARA listed species
listed_species <- read.csv("../Data/NaturalResources/Species/MAR_listed_species.csv",
                           stringsAsFactors = FALSE)
listed_species <- listed_species %>% rename("SARA status"=Schedule.status,
                                            "COSEWIC listing"=COSEWIC.status,
                                            "Wild Species listing"=Wild_Species,
                                            "SCIENTIFICNAME"=Scientific_Name_upper,
                                            "COMMONNAME"=Common_Name_upper,
                                            "Scientific Name"=Scientific_Name,
                                            "Common Name"=Common_Name)

####### Species Lists  #######
cetacean_list <- c("BELUGA WHALE", "NORTH ATLANTIC RIGHT WHALE", "FIN WHALE", 
                   "NORTHERN BOTTLENOSE WHALE",
                   "HARBOUR PORPOISE", "KILLER WHALE", "BLUE WHALE", "SEI WHALE", "SOWERBY'S BEAKED WHALE")


other_species_list <- c("LOGGERHEAD SEA TURTLE", "ATLANTIC WALRUS", "HARBOUR SEAL LACS DES LOUPS MARINS SUBSPECIES", "LEATHERBACK SEA TURTLE")
listed_cetacean_species <- subset(listed_species, COMMONNAME %in% cetacean_list)
listed_other_species <- subset(listed_species, COMMONNAME %in% other_species_list)
listed_fish_invert_species <- listed_species[ ! listed_species$COMMONNAME %in% c(other_species_list,cetacean_list), ]

# Legend file for displaying cetacean results in consistent colours
Legend <- read.csv("../Data/NaturalResources/Species/Cetaceans/CetaceanLegend.csv", stringsAsFactors = FALSE)
Legend <- dplyr::rename(Legend,c("Scientific Name" = "Scientific_Name"))

# Ocean Biogeographic Information System (OBIS)
obis <- read.csv("../Data/NaturalResources/Species/OBIS_GBIF_iNaturalist/OBIS_MAR_priority_records.csv", stringsAsFactors = FALSE)
obis <- dplyr::select(obis,scientificName, decimalLatitude, decimalLongitude, year)
obis <- obis %>% transmute(obis, scientificName = str_to_sentence(scientificName))
obis <- obis %>% rename("Scientific Name"= scientificName, "YEAR" = year)
obis <- obis %>% dplyr::filter(YEAR >= 2010)

# OBIS fish and inverts
obis_fish <- merge(obis, listed_fish_invert_species, by='Scientific Name')
obis_fish <- dplyr::select(obis_fish,"Scientific Name", YEAR, "Common Name", "COSEWIC listing",
                           "SARA status", decimalLatitude, decimalLongitude)
obis_fish_sf <- st_as_sf(obis_fish, coords = c("decimalLongitude","decimalLatitude"), crs = 4326)

# OBIS cetaceans
obis_cet <- merge(obis, Legend, by='Scientific Name')
obis_cet <- dplyr::select(obis_cet,"Scientific Name", YEAR, Legend,
                          decimalLatitude, decimalLongitude)
obis_cet_sf <- st_as_sf(obis_cet, coords = c("decimalLongitude","decimalLatitude"), crs = 4326)

# RV survey data
RVList <-  SelectRV_fn(SurveyPrefix, File, minYear)
RVCatch_sf <- RVList[[1]]
RVCatch_sf <- dplyr::select(RVCatch_sf, CODE, YEAR, ELAT, ELONG, TOTNO, geometry)
RVGSSPECIES <- RVList[[2]]

# Species at Risk critical habitat
ClippedCritHab_sf <- st_read("../Data/NaturalResources/Species/SpeciesAtRisk/clipped_layers/ClipCritHab.shp", stringsAsFactors = FALSE)
ClippedCritHab_sf <- st_make_valid(ClippedCritHab_sf)

#Northern Bottlenose Whale Important Habitat
NBNW_ImpHab_sf <- st_read(dsn = "../Data/NaturalResources/Species/Cetaceans/NorthernBottlenoseWhale_FGP/NorthernBottlenose.gdb", layer = "NorthernBottlenoseWhale_InterCanyonHabitat", stringsAsFactors = FALSE)
NBNW_ImpHab_sf <- st_make_valid(NBNW_ImpHab_sf)

# Species Distribution Model (SDM) outputs
fin_whale <- raster("../Data/NaturalResources/Species/Cetaceans/PriorityAreas_FGP/Fin_Whale.tif")
fin_whale[fin_whale==0] <- NA
# fin_whale_sp <- rasterToPolygons(fin_whale, na.rm=TRUE, dissolve=FALSE)

harbour_porpoise <- raster("../Data/NaturalResources/Species/Cetaceans/PriorityAreas_FGP/Harbour_Porpoise.tif")
harbour_porpoise[harbour_porpoise==0] <- NA

humpback_whale <- raster("../Data/NaturalResources/Species/Cetaceans/PriorityAreas_FGP/Humpback_Whale.tif")
humpback_whale[humpback_whale==0] <- NA

sei_whale <- raster("../Data/NaturalResources/Species/Cetaceans/PriorityAreas_FGP/Sei_Whale.tif")
sei_whale[sei_whale==0] <- NA

#Read Blue Whale Important Habitat shapefile and Project to WGS84
Blue_32198 <- st_read("../Data/NaturalResources/Species/Cetaceans/BlueWhaleHabitat_FGP/BlueWhaleHabitat_HabitatBaleineBleue.shp", quiet=TRUE, stringsAsFactors = FALSE)
Blue_Whale_sf <- st_transform(Blue_32198, crs = 4326)
Blue_Whale_sf <- setNames(Blue_Whale_sf, replace(names(Blue_Whale_sf), names(Blue_Whale_sf) == 'activité', 'activite'))
Blue_Whale_sf$activity[Blue_Whale_sf$activity == "foraging/Feeding"] <- "Foraging/Feeding"
Blue_Whale_sf$activity[Blue_Whale_sf$activity == "Migrant"] <- "Migration"
Blue_Whale_sf$months[Blue_Whale_sf$months == "all year"] <- "All year"
Blue_Whale_sf$months[Blue_Whale_sf$months == "December to February/March to May"] <- "Dec-Feb/Mar-May"
Blue_Whale_sf$months[Blue_Whale_sf$months == "December to February/June to August"] <- "Dec-Feb/Jun-Aug"
Blue_Whale_sf$months[Blue_Whale_sf$months == "March to May/June to August"] <- "Mar-May/Jun-Aug"
Blue_Whale_sf$Activity <- paste(Blue_Whale_sf$activity,"-",Blue_Whale_sf$months)
BlueWhale_ImpHab_sf <- Blue_Whale_sf
BlueWhale_ImpHab_sf <- st_make_valid(BlueWhale_ImpHab_sf)

# Ecologically or Biologically Significant Marine Areas (EBSAs)
EBSA_sf <- st_read("../Data/Zones/DFO_EBSA_FGP/DFO_EBSA.shp")
EBSA_sf <- st_transform(EBSA_sf, crs = 4326)
EBSA_sf$Report_URL <- str_replace(EBSA_sf$Report_URL, ".pdf", ".html")
EBSA_sf <- st_make_valid(EBSA_sf)

# Save all objects to a single .Rdata file 

save(bioregion_sf, BlueWhale_ImpHab_sf, bounds_sf, ClippedCritHab_sf, EBSA_sf, fin_whale, 
     harbour_porpoise, humpback_whale, land10m_sf, land50k_sf, 
     listed_species, listed_cetacean_species, listed_other_species, listed_fish_invert_species,
     NBNW_ImpHab_sf, obis_cet_sf, obis_fish_sf, rockweed_sf, 
     RVCatch_sf, RVGSSPECIES, sei_whale, 
     file = "../Data/Rdata/OpenData.RData")


# Species at Risk distribution
sardist_sf <- st_read("../Data/NaturalResources/Species/SpeciesAtRisk/clipped_layers/sardist_4326.shp", stringsAsFactors = FALSE)
# sardist_sf <- dplyr::select(sardist_sf,Common_Nam, Population, Scientific)
sardist_sf <- st_make_valid(sardist_sf)

save(sardist_sf, file = "../Data/Rdata/OpenData_sardist.RData")


########################################################################-
# save secure data as single .Rdata file

# Cetacean legend file
Legend <- read.csv("../Data/NaturalResources/Species/Cetaceans/CetaceanLegend.csv", stringsAsFactors = FALSE)
Legend <- dplyr::rename(Legend,c("Scientific Name" = "Scientific_Name"))

# Cetacean point data  #########################
# Whale Sightings Database (wsdb)
wsdb <- read.csv("../Data/NaturalResources/Species/Cetaceans/WSDB/MarWSDB_20210407.csv", stringsAsFactors = FALSE)
wsdb <- dplyr::select(wsdb,COMMONNAME,SCIENTIFICNAME,YEAR,LATITUDE,LONGITUDE)
wsdb <- wsdb %>% dplyr::filter(YEAR >= 2010)
wsdb <- dplyr::rename(wsdb,c("Scientific Name" = "SCIENTIFICNAME",
                             "CNAME"= COMMONNAME))
wsdb <- merge(wsdb, Legend, by='Scientific Name')
wsdb <- dplyr::select(wsdb,CNAME,'Scientific Name',YEAR,Legend, LATITUDE,LONGITUDE)
# wsdb <- dplyr::rename(wsdb,c("COMMONNAME" = "CNAME"))
wsdb_sf <- st_as_sf(wsdb, coords = c("LONGITUDE","LATITUDE"), crs = 4326)

# Whitehead lab
whitehead <- read.csv("../Data/NaturalResources/Species/Cetaceans/Whitehead_Lab/whitehead_lab.csv", stringsAsFactors = FALSE)
whitehead$YEAR <- lubridate::year(whitehead$Date)
whitehead <- whitehead %>% dplyr::filter(YEAR >= 2010)
whitehead <- whitehead %>% rename("Scientific Name"= species.name)
whitehead <- merge(whitehead, Legend, by='Scientific Name')
whitehead <- dplyr::select(whitehead,'Scientific Name', YEAR, Legend, Lat, Long)
# correct the longitude values to be negative
whitehead$Long <- -1*whitehead$Long
whitehead_sf <- st_as_sf(whitehead, coords = c("Long","Lat"), crs = 4326)

# North Atlantic Right Whale Consortium (narwc)
narwc <- read.csv("../Data/NaturalResources/Species/Cetaceans/NARWC/NARWC_09-18-2020.csv", stringsAsFactors = FALSE)
narwcspecies <-  read.csv("../Data/NaturalResources/Species/Cetaceans/NARWC/NARWCSpeciesNames.csv", stringsAsFactors = FALSE)
narwcspecies <- narwcspecies %>% rename("Scientific Name"= ScientificName)
narwc <- merge(narwc, narwcspecies, by='SPECNAME')
narwc <- narwc %>% dplyr::filter(YEAR >= 2010)
narwc <- merge(narwc, Legend, by = 'Scientific Name')
narwc <- dplyr::select(narwc,'Scientific Name', YEAR, Legend, LATITUDE, LONGITUDE)
narwc_sf <- st_as_sf(narwc, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
# END point data   #########################


# Leatherback turtle habitat
leatherback_sf <- st_read("../Data/NaturalResources/Species/SpeciesAtRisk/LeatherBackTurtleCriticalHabitat/LBT_CH_2013.shp", stringsAsFactors = FALSE)
leatherback_sf <- st_make_valid(leatherback_sf)

# ISDB and MARFIS data
load("../Data/mar.wrangling/isdb.RData")
isdb_sf <- na.omit(isdb_sf) # remove NA values (some in SPECCID).  this is a bit slow

load("../Data/mar.wrangling/marfis.RData")

# Load the ISDB and MARFIS species tables
load("../Data/mar.wrangling/MARFIS.SPECIES.RData")
load("../Data/mar.wrangling/ISDB.ISSPECIESCODES.RData")

SPECIES <- dplyr::select(SPECIES, 
                         one_of(c("SPECIES_CODE","SPECIES_NAME")))
ISSPECIESCODES <- dplyr::select(ISSPECIESCODES, 
                                one_of(c("SPECCD_ID", "COMMON","SCIENTIFIC")))

ISSPECIESCODES <- ISSPECIESCODES %>% transmute(ISSPECIESCODES, SCIENTIFIC=str_to_sentence(SCIENTIFIC))
ISSPECIESCODES <- ISSPECIESCODES %>% transmute(ISSPECIESCODES, COMMON=str_to_sentence(COMMON))
ISSPECIESCODES <- ISSPECIESCODES %>% rename("Common Name"= COMMON,
                                            "Scientific Name" = SCIENTIFIC)

MARFISSPECIESCODES <- SPECIES %>% rename("COMMONNAME"= SPECIES_NAME)


# Save all objects to a single .Rdata file
save(isdb_sf,ISSPECIESCODES,leatherback_sf,Legend, marfis_sf,MARFISSPECIESCODES,narwc_sf, whitehead_sf, wsdb_sf,
     file = "../Data/Rdata/SecureData.Rdata")

########################################################-