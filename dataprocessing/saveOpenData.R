
source(here::here("dataprocessing/openDataHelpers.R"))


# ----------EBSA------------
EBSApkgId <- "d2d6057f-d7c4-45d9-9fd9-0a58370577e0"
EBSAresId <- "ec990fd7-91b0-4dbb-a0f4-bb11070a84c1"

EBSA_rr <- get_opendata_rr(pkgId, resId)
EBSA_rr$data_sf$Report_URL <- str_replace(EBSA_rr$data_sf$Report_URL, ".pdf", ".html")


# -----------SAR DIST0--------------

sardistPkgId <- "e0fabad5-9379-4077-87b9-5705f28c490b"
EBSAresId <- "ec990fd7-91b0-4dbb-a0f4-bb11070a84c1"

EBSA_rr <- get_opendata_rr(pkgId, resId)
EBSA_rr$data_sf$Report_URL <- str_replace(EBSA_rr$data_sf$Report_URL, ".pdf", ".html")