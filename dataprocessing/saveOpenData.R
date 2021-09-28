
source(here::here("dataprocessing/openDataHelpers.R"))


# ----------------EBSA----------------- #good
EBSApkgId <- "d2d6057f-d7c4-45d9-9fd9-0a58370577e0"
EBSAresId <- "ec990fd7-91b0-4dbb-a0f4-bb11070a84c1"

EBSA_rr <- get_opendata_rr(EBSApkgId, EBSAresId)
EBSA_rr$data_sf$Report_URL <- str_replace(EBSA_rr$data_sf$Report_URL, ".pdf", ".html")


# -----------SAR DIST--------------

sardistPkgId <- "e0fabad5-9379-4077-87b9-5705f28c490b"
sardistResId <- "84f84608-d045-4305-b5a3-47f22281a5f1"

# sardist_rr <- get_opendata_rr(sardistPkgId, sardistResId)



# -----------CritHab-------------- # check table cols.
crithabPkgId <- "db177a8c-5d7d-49eb-8290-31e6a45d786c"
crithabResId <- "394df1b9-0c01-476b-b9e3-8abbf4559623"
critHabLayer <- "DFO_SARA_CritHab_2021_FGP_EN"
crithab_rr <- get_opendata_rr(crithabPkgId, crithabResId, gdbLayer = critHabLayer)
crithab_rr$data_sf$Common_Nam <- crithab_rr$data_sf$Common_Name_EN


# -----------SDM--------------
sdmPkgId <- "c094782e-0d6f-4cc0-b5a3-58908493a433"
sdmResId <- "1ac4df6a-ba45-4f13-a488-bd12e90a8bc7"
sdmLayer <- "StudyArea"

#sdm_rr <- get_opendata_rr(sdmPkgId, sdmResId, gdbLayer = sdmLayer)


# -----------NBW-------------- # good
nbwPkgId <- "9fd7d004-970c-11eb-a2f3-1860247f53e3"
nbwResId <- "f69a7d34-7c18-485b-98d7-8d45b7f8a3ce"
nbwLayer <- "NorthernBottlenoseWhale_InterCanyonHabitat"
nbw_rr <- get_opendata_rr(nbwPkgId, nbwResId, gdbLayer = nbwLayer)


# -----------BW hab-------------- # good
bwhabPkgId <- "8fafd919-fcbe-43a3-a911-3d9461273441"
bwResId <- "3af8ad03-c0da-4cfa-940d-d757c0c24cb7"

bwhab_rr <- get_opendata_rr(bwhabPkgId, bwResId)
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




save(EBSA_rr, 
     #sardist_rr,
     crithab_rr, 
     #sdm_rr, 
     nbw_rr, 
     bwhab_rr,
     file = here::here("app/data/TESTOpenData.RData"))

