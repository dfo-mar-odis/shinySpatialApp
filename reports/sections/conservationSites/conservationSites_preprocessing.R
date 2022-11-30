source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "conservationSites_rr"), regionStr)

# -------------Marine Protected Areas (mpa)---------------------
conservationSites_raw <- sf::st_read(file.path(fileLoadPath, "Management/MPAN-Draft/MPAN_DraftDesign_Maritimes/MPAN_DraftDesign_Maritimes.shp"), stringsAsFactors = FALSE)
conservationSites_sf <- sf::st_transform(conservationSites_raw, crs = 4326)
conservationSites_sf <- sf::st_make_valid(conservationSites_sf)
conservationSites_sf$Legend <- as.factor(conservationSites_sf$STATUS)
levels(conservationSites_sf$Legend) <- list("Existing Network Site"="Existing", "Other Network Sites"="Proposed", "Area of Interest for Oceans Act MPA"="Proposed AOI", "Proposed Conservation Area Under the Fisheries Act"="Proposed SBA")
conservationSites_sf$Legend <- as.character(conservationSites_sf$Legend)
conservationSites_sf[conservationSites_sf$Id == 18, "Legend" ] <- "Bras d'Or Lake -sites to be determined"

conservationSites_rr <- list("title" = "Draft Conservation Network Design",
                             "data_sf" = conservationSites_sf,
                             "attribute" = "Legend",
                             "metadata" = read_google_metadata("conservationSites_rr")
)

save(conservationSites_rr, file = file.path(localFileSavePath, "Secure/conservationSites_rr.RData"))

