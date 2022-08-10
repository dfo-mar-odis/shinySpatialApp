# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "sarsearch_rr"), regionStr)

# ---------------------SAR Search-----------------------------------
# LOADING AND PROCESSING CODE HERE
library(rvest)
s <- rvest::session("127.0.0.1:8000/en")
s <- rvest::session_jump_to(s, "http://127.0.0.1:8000/en/accounts/login/")
loginForm <- html_form(s)[[2]]
httr::cookies(s)

filledForm <- html_form_set(loginForm, email = "quentin.stoyel@dfo-mpo.gc.ca")



html_form_submit(filledForm)



url <- "http://dmapps/api/sar-search/points/?limit=5&f=json"
res <- httr::GET(url)
raise <- content(res, as="text")
#parse JSON
sarsearch_df <- jsonlite::fromJSON(raise)

# rr object structure:
sarsearch_rr <- list("title" = "DMapps SAR Search Database",
                     "data_sf" = "an sf object",
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

