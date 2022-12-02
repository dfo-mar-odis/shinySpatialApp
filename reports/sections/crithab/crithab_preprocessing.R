source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "crithab_rr", "leatherback_rr"), regionStr)
# -----------CritHab-------------- # check table cols.
crithabPkgId <- "db177a8c-5d7d-49eb-8290-31e6a45d786c"
crithabResId <- "94284a7f-46b8-4304-99d5-c278e88b22a8"
critHabLayer <- "DFO_SARA_CritHab_2022_FGP_EN"

crithab_rr <- get_opendata_rr(crithabPkgId, crithabResId, 
                                  region_sf = region_sf,
                                  gdbLayer = critHabLayer, 
                                  checkDate = critHabCheckDate)

crithab_rr$metadata <- read_google_metadata("crithab_rr", isOpenData = TRUE)
crithab_rr$attribute <- "Common_Name_EN"
save(crithab_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/crithab_rr.RData"))



# ------------LEATHERBACKS--------------------

# Leatherback turtle habitat
leatherback_sf <- sf::st_read(file.path(fileLoadPath, "NaturalResources/Species/SpeciesAtRisk/LeatherBackTurtleCriticalHabitat/LBT_CH_2013.shp"), stringsAsFactors = FALSE)
leatherback_sf <- sf::st_make_valid(leatherback_sf)
leatherback_sf <- sf::st_crop(leatherback_sf, region_sf)


leatherback_rr <- list("title" = " Leatherback Sea Turtle draft critical habitat",
                       "data_sf" = leatherback_sf,
                       "attribute" = "NONE",
                       "metadata" = read_google_metadata("leatherback_rr")
)
save(leatherback_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Secure/leatherback_rr.RData"))

# -------------CRITHAB from SDE--------------
library(sf)
library(gdalUtilities)
# grabbed this from stack overflow, converts multisurfaces to multipolygons
ensure_multipolygons <- function(X) {
  tmp1 <- tempfile(fileext = ".gpkg")
  tmp2 <- tempfile(fileext = ".gpkg")
  st_write(X, tmp1)
  ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
  Y <- st_read(tmp2)
  st_sf(st_drop_geometry(X), geom = st_geometry(Y))
}

crithabPkgId <- "db177a8c-5d7d-49eb-8290-31e6a45d786c"
crithab_gdb <- here::here("../../sara_database/my_data/crithab.gdb/")
national_list <- here::here("../../sara_database/my_data/national_list.csv")

nationalList <- read.csv(national_list)
nationalList <- select(nationalList, c(Common.Name, SPECIES_ID, Population, SARA.Status, COSEWIC.Status))

critHabCheckDate <-  get_check_date("crithab_rr")
crithab_rr <- get_opendata_rr(crithabPkgId)
rawCrithab_sf <- sf::st_read(crithab_gdb)

crithab_sf <- ensure_multipolygons(rawCrithab_sf)

crithab_sf <- sf::st_transform(crithab_sf, 4326)
crithab_sf <- sf::st_intersection(crithab_sf, region_sf)
crithab_sf <- sf::st_make_valid(crithab_sf)

crithab_sf <- dplyr::left_join(crithab_sf, nationalList, by="SPECIES_ID")
crithab_sf <- select(crithab_sf, c(Common.Name, WATERBODY, Population, SARA.Status, 
                                   COSEWIC.Status, CHSTATUS_E, geom))
names(crithab_sf) <- c("Common_Name_EN", "Waterbody", "Population_EN", "SARA_Status",
                       "COSEWIC_Status", "Area_Status", "geom")


crithab_rr$data_sf <- crithab_sf
crithab_rr$attribute <- "Common_Name_EN"
crithab_rr$metadata <- read_google_metadata("crithab_rr")

save(crithab_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/crithab_rr.RData"))



