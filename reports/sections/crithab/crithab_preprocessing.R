source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))


loadResult <- load_rdata(c("CommonData", "crithab_rr", "draftCritHab_rr"), regionStr)

# -------------CRITHAB from SDE--------------
crithabPkgId <- "db177a8c-5d7d-49eb-8290-31e6a45d786c"
crithab_rr <- get_opendata_rr(crithabPkgId)

draftCritHab_rr <- list("title" = "Draft Critical Habitat")

if (globalControlEnv$updateGeoms) {
    
  # grabbed this from stack overflow, converts multisurfaces to multipolygons
  ensure_multipolygons <- function(X) {
    tmp1 <- tempfile(fileext = ".gpkg")
    tmp2 <- tempfile(fileext = ".gpkg")
    sf::st_write(X, tmp1)
    gdalUtilities::ogr2ogr(tmp1, tmp2, f = "GPKG", nlt = "MULTIPOLYGON")
    Y <- sf::st_read(tmp2)
    sf::st_sf(sf::st_drop_geometry(X), geom = sf::st_geometry(Y))
  }

  national_list <- file.path(fileLoadPath, "/NaturalResources/Species/SpeciesAtRisk/SaraDatabase/national_list.csv")
  nationalList <- read.csv(national_list)
  nationalList <- select(nationalList, c(Common.Name, SPECIES_ID, Population, SARA.Status, COSEWIC.Status))
  
  crithab_gdb <- file.path(fileLoadPath, "/NaturalResources/Species/SpeciesAtRisk/SaraDatabase/crithab.gdb")
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
  
  finalCrithab_sf <- dplyr::filter(crithab_sf, Area_Status == "Final")
  crithab_rr$data_sf <- finalCrithab_sf
  
  draft_sf <- dplyr::filter(crithab_sf, Area_Status == "Draft")
  draftCritHab_rr$data_sf <- draft_sf
}

crithab_rr$attribute <- "Common_Name_EN"
crithab_rr$metadata <- read_google_metadata("crithab_rr")
save(crithab_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/crithab_rr.RData"))


draftCritHab_rr$attribute <- "Common_Name_EN"
draftCritHab_rr$metadata <- read_google_metadata("draftCritHab_rr")
save(draftCritHab_rr, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/draftCritHab_rr.RData"))



