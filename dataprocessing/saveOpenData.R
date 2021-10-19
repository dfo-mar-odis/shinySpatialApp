source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

region_sf <- st_read(here::here("app/studyAreaTest/geoms_slc_MarBioRegion.geojson"))
setwd(here::here("app/data/MAR"))

loadResult <- load_rdata(c("EBSA_rr", "crithab_rr", "sardist_rr", "sdm_rr", 
                           "nbw_rr", "bwhab_rr"),  "MAR")

highQuality <- list("en" = "High", "fr" = "Élevée")
noneList <- list("en" = "None", "fr"= "Aucun")

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
  EBSA_rr$contact <- "[carissa.philippe\\@dfo-mpo.gc.ca](mailto:carissa.philippe@dfo-mpo.gc.ca){.email}"
  save(EBSA_rr, file = "./Open/EBSA_rr.RData")
}


# -----------SAR DIST--------------

sardistPkgId <- "e0fabad5-9379-4077-87b9-5705f28c490b"
sardistResId <- "84f84608-d045-4305-b5a3-47f22281a5f1"
sardistLayer <- "DFO_SARA_Dist_2021_FGP_EN"

sardistCheckDate <-  get_check_date("sardist_rr")

openSardist_rr <- get_opendata_rr(sardistPkgId, sardistResId, 
                                  region_sf = region_sf,
                                  gdbLayer = sardistLayer,
                                  checkDate = sardistCheckDate)
if(!is.null(openSardist_rr)) {
  sardist_rr <- openSardist_rr
  save(sardist_rr, file = "./Open/sardist_rr.RData")
}


# -----------CritHab-------------- # check table cols.
crithabPkgId <- "db177a8c-5d7d-49eb-8290-31e6a45d786c"
crithabResId <- "394df1b9-0c01-476b-b9e3-8abbf4559623"
critHabLayer <- "DFO_SARA_CritHab_2021_FGP_EN"

critHabCheckDate <-  get_check_date("crithab_rr")

openCrithab_rr <- get_opendata_rr(crithabPkgId, crithabResId, 
                                  region_sf = region_sf,
                                  gdbLayer = critHabLayer, 
                                  checkDate = critHabCheckDate)
if(!is.null(openCrithab_rr)) {
  crithab_rr <- openCrithab_rr
  crithab_rr$data_sf$Common_Nam <- crithab_rr$data_sf$Common_Name_EN
  save(crithab_rr, file = "./Open/crithab_rr.RData")
}


# -----------SDM--------------
sdmPkgId <- "c094782e-0d6f-4cc0-b5a3-58908493a433"
sdmResId <- "1ac4df6a-ba45-4f13-a488-bd12e90a8bc7"
sdmLayer <- "StudyArea"
sdmCheckDate <-  get_check_date("sdm_rr")

openSdm_rr <- get_opendata_rr(sdmPkgId, sdmResId, region_sf = region_sf,
                              gdbLayer = sdmLayer, checkDate = sdmCheckDate)
if(!is.null(openSdm_rr)) {
  sdm_rr <- openSdm_rr
  save(sdm_rr, file = "./Open/sdm_rr.RData")
}

# -----------NBW-------------- 
nbwPkgId <- "9fd7d004-970c-11eb-a2f3-1860247f53e3"
nbwResId <- "f69a7d34-7c18-485b-98d7-8d45b7f8a3ce"
nbwLayer <- "NorthernBottlenoseWhale_InterCanyonHabitat"

nbwCheckDate <- get_check_date("nbw_rr")

openNbw_rr <- get_opendata_rr(nbwPkgId, nbwResId, region_sf = region_sf,
                              gdbLayer = nbwLayer, checkDate = nbwCheckDate)
if(!is.null(openNbw_rr)) {
  nbw_rr <- openNbw_rr
  save(nbw_rr, file = "./Open/nbw_rr.RData")
}


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
  save(bwhab_rr, file = "./Open/bwhab_rr.RData")
}

