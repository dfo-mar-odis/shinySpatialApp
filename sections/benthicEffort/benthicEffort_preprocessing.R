source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

source(here::here("config.R"))

loadResult <- load_rdata(c("CommonData", "benthicEffort_rr"), regionStr)


# -----------Benthic Fishing Effort--------------

benthicEffortPkgId <- "273df20a-47ae-42c0-bc58-01e451d4897a"
benthicEffortResId <- "fc685885-9ffe-4a7c-a0e1-78119ce68e98"
benthicEffortCheckDate <-  get_check_date("benthicEffort_rr")


ea_rr <- get_opendata_rr(benthicEffortPkgId, benthicEffortResId, region_sf = region_sf, tifFile = "EA_All_fisheries_Combined_Percentiles.tif", returnRaster=TRUE)
gsl_rr <- get_opendata_rr(benthicEffortPkgId, benthicEffortResId, region_sf = region_sf, tifFile = "GSL_All_fisheries_Combined_Percentiles.tif", returnRaster=TRUE )
nl_rr <- get_opendata_rr(benthicEffortPkgId, benthicEffortResId, region_sf = region_sf, tifFile = "NL_All_fisheries_Combined_Percentiles.tif", returnRaster=TRUE )
ss_rr <- get_opendata_rr(benthicEffortPkgId, benthicEffortResId, region_sf = region_sf, tifFile = "SS_All_fisheries_Combined_Percentiles.tif", returnRaster=TRUE )

allRasters <- c(ea_rr$data_sf, gsl_rr$data_sf, nl_rr$data_sf, ss_rr$data_sf)
minResIndx <- which.min(sapply(allRasters, xres))
allRasters <- sapply(allRasters, projectRaster, allRasters[[minResIndx]])

firstOverlay <- overlay(allRasters[[1]], allRasters[[2]], fun=function(x, y) ifelse(x==0 | is.na(x), y, x))
secondOverlay <- overlay(firstOverlay, allRasters[[3]], fun=function(x, y) ifelse(x==0 | is.na(x), y, x))
plot(secondOverlay)
