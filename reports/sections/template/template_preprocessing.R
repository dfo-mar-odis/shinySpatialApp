# contains functions for downloading open data records
source(here::here("reports/R/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "template_rr"), regionStr)

# ---------------------TEMPLATE-----------------------------------
# LOADING AND PROCESSING CODE HERE

# rr object structure:
template_rr <- list("title" = "Template Title","data_sf" = "an sf object",
              "attribute" = "Column to plot, or NONE",
              "metadata" = list("contact" = email_format("email@email.com"), 
                                "accessedOnStr" = list("en" ="January 1, 2022", "fr" = "1 Janvier, 2022") ,
                                "accessDate" = as.Date("2022-01-01"),
                                "searchYears" = "2022-2022",
                                "securityLevel" = noneList,
                                "qualityTier" = highQuality,
                                "constraints" = internalUse
              )
              
)
save(template_rr, file = file.path(localFileSavePath, "Secure/template_rr.RData"))

