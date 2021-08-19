### MARFIS EXTRACTION

library(Mar.datawrangling)
library(Mar.utils)

extract_marfis <- function(){
  #do the extraction of 2015 data
  get_data('marfis', data.dir=data.dir)
  PRO_SPC_INFO <- PRO_SPC_INFO[PRO_SPC_INFO$YEAR == 2015,]
  self_filter()
  #summarize it and clean out all we don't need
  marf2015<- summarize_catches()
  neededFields <-c("RND_WEIGHT_KGS", "VR_NUMBER_FISHING","VR_NUMBER_LANDING",
                   "LICENCE_ID", "BUYER_ID", "SPECIES_CODE", "MON_DOC_ID",
                   "LATITUDE","LONGITUDE")
  marf2015_cln<- marf2015[,neededFields]
  #aggregate it
  marf2015_DemoHex = assess_privacy(df=marf2015_cln, agg.fields = "RND_WEIGHT_KGS", calculate = c("MEAN", "SUM"), for.public = T,
                                    lat.field = "LATITUDE", lon.field = "LONGITUDE",
                                    sens.fields = c("VR_NUMBER_FISHING","VR_NUMBER_LANDING","LICENCE_ID", "BUYER_ID"), 
                                    facet.field = "SPECIES_CODE", key.fields = c("MON_DOC_ID"), 
                                    create.shps = T, file.id = "marf2015_DemoHex")
  return(marf2015_DemoHex)
  }

extract_isdb <- function(){
  ### ISDB EXTRACTION
  
  get_data('isdb', data.dir=data.dir)
  ISSETPROFILE_WIDE <- ISSETPROFILE_WIDE[ISSETPROFILE_WIDE$YEAR == 2015,]
  self_filter()
  isdb2015<- summarize_catches()
  neededFieldsI <-c("LICENSE_NO", "MARFIS_LICENSE_NO","FISHSET_ID", "EST_NUM_CAUGHT", "EST_COMBINED_WT", "SPECCD_ID",
                    "LATITUDE","LONGITUDE")
  SPECIES_ISDB <- unique(isdb2015[,c("SPECCD_ID", "COMMON")])
  isdb2015_cln<- isdb2015[,neededFieldsI]
  isdb2015_cln[c("EST_NUM_CAUGHT", "EST_COMBINED_WT","MARFIS_LICENSE_NO")][is.na(isdb2015_cln[c("EST_NUM_CAUGHT", "EST_COMBINED_WT","MARFIS_LICENSE_NO")])] <-0
  isdb2015_DemoHex = assess_privacy(df=isdb2015_cln, agg.fields = c("EST_NUM_CAUGHT", "EST_COMBINED_WT"), calculate = c("SUM"), for.public = T,
                                    lat.field = "LATITUDE", lon.field = "LONGITUDE",
                                    sens.fields = c("LICENSE_NO", "MARFIS_LICENSE_NO"), 
                                    facet.field = "SPECCD_ID", key.fields = c("FISHSET_ID"), 
                                    create.shps = T, file.id = "isdb2015_DemoHex", ignore.col.limit = T)
  return(isdb2015_DemoHex)
}
