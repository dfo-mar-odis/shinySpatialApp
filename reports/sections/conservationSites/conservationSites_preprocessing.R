source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "conservationSites_rr"), regionStr)

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

save(conservationSites_rr, file = file.path(localFileSavePath, "Secure/conservationSites_rr.RData"))

