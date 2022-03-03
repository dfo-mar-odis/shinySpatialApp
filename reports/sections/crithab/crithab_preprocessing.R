source(here::here("reports/R/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "crithab_rr", "leatherback_rr"), regionStr)

# -----------CritHab-------------- # check table cols.
crithabPkgId <- "db177a8c-5d7d-49eb-8290-31e6a45d786c"
crithabResId <- "94284a7f-46b8-4304-99d5-c278e88b22a8"
critHabLayer <- "DFO_SARA_CritHab_2021_FGP_EN"

critHabCheckDate <-  get_check_date("crithab_rr")

openCrithab_rr <- get_opendata_rr(crithabPkgId, crithabResId, 
                                  region_sf = region_sf,
                                  gdbLayer = critHabLayer, 
                                  checkDate = critHabCheckDate)

if(!is.null(openCrithab_rr)) {
  crithab_rr <- openCrithab_rr
  crithab_rr$metadata$qualityTier <- highQuality
  crithab_rr$metadata$contact <- email_format("info@dfo-mpo.gc.ca")
  crithab_rr$attribute <- "Common_Nam"
  crithab_rr$data_sf$Common_Nam <- crithab_rr$data_sf$Common_Name_EN
  save(crithab_rr, file = file.path(localFileSavePath, "Open/crithab_rr.RData"))
}



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
save(leatherback_rr, file = file.path(localFileSavePath, "Secure/leatherback_rr.RData"))

