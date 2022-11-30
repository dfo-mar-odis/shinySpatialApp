source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))
source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "benthicEffort_rr"), regionStr)


# -----------Benthic Fishing Effort--------------

benthicEffortPkgId <- "273df20a-47ae-42c0-bc58-01e451d4897a"
benthicEffortResId <- "9473a44f-de4b-4177-a77a-d7048e252079"
benthicEffortCheckDate <-  get_check_date("benthicEffort_rr")


ea_rr <- get_opendata_rr(benthicEffortPkgId, benthicEffortResId, region_sf = region_sf, tifFile = "EA_All_fisheries_Combined_Percentiles.tif", returnRaster=TRUE)
gsl_rr <- get_opendata_rr(benthicEffortPkgId, benthicEffortResId, region_sf = region_sf, tifFile = "GSL_All_fisheries_Combined_Percentiles.tif", returnRaster=TRUE )
nl_rr <- get_opendata_rr(benthicEffortPkgId, benthicEffortResId, region_sf = region_sf, tifFile = "NL_All_fisheries_Combined_Percentiles.tif", returnRaster=TRUE )
ss_rr <- get_opendata_rr(benthicEffortPkgId, benthicEffortResId, region_sf = region_sf, tifFile = "SS_All_fisheries_Combined_Percentiles.tif", returnRaster=TRUE )

allRasters <- c(ea_rr$data_sf, gsl_rr$data_sf, nl_rr$data_sf, ss_rr$data_sf)
minResIndx <- which.min(sapply(allRasters, xres))
allRasters <- sapply(allRasters, projectRaster, allRasters[[minResIndx]])

if (length(allRasters) > 0){
  mergeRaster <- allRasters[[1]]
  for (rasterIndex in 2:length(allRasters)) {
    mergeRaster <- overlay(mergeRaster, allRasters[[rasterIndex]], 
                           fun=function(x, y) ifelse(x==0 | is.na(x), y, x))
  }
} else {
  mergeRaster <- NULL
}

benthicEffort_rr <- ea_rr
benthicEffort_rr$metadata$qualityTier <- mediumQuality
benthicEffort_rr$metadata$pipelinePath = paste0(githubRepo, "reports/sections/benthicEffort/benthicEffort_preprocessing.R")

benthicEffort_rr$data_sf <- mergeRaster

save(benthicEffort_rr, file = file.path(localFileSavePath, "Open/benthicEffort_rr.RData"))

