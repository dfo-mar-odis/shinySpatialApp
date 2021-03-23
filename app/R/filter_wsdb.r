# filter_wsdb <- function(wsdb) {
# 
#   wsdb_filt <- wsdb[wsdb$COMMONNAME %in% c('PORPOISE-HARBOUR', 'WHALE-SEI','WHALE-FIN', 'WHALE-NORTH ATLANTIC RIGHT',
#                                     'WHALE-NORTHERN BOTTLENOSE', 'WHALE-KILLER', 'WHALE-BLUE', "WHALE-SOWERBY'S BEAKED"), ]
# 
#   wsdb_filt$COMMONNAME[which(wsdb_filt$COMMONNAME=="PORPOISE-HARBOUR")]= "Harbour Porpoise: Threatened (SARA) Special Concern (COSEWIC)"
#   wsdb_filt$COMMONNAME[which(wsdb_filt$COMMONNAME=="WHALE-FIN")]= "Fin Whale: Special Concern (SARA & COSEWIC)"
#   wsdb_filt$COMMONNAME[which(wsdb_filt$COMMONNAME=="WHALE-NORTH ATLANTIC RIGHT")]= "North Atlantic Right Whale: Endangered (SARA & COSEWIC)"
#   wsdb_filt$COMMONNAME[which(wsdb_filt$COMMONNAME=="WHALE-NORTHERN BOTTLENOSE")]= "Northern Bottlenose Whale: Endangered (SARA & COSEWIC)"
#   wsdb_filt$COMMONNAME[which(wsdb_filt$COMMONNAME=="WHALE-KILLER")]= "Killer Whale: No Status (SARA) & Special Concern (COSEWIC)"
#   wsdb_filt$COMMONNAME[which(wsdb_filt$COMMONNAME=="WHALE-BLUE")]= "Blue Whale: Endangered (SARA & COSEWIC)"
#   wsdb_filt$COMMONNAME[which(wsdb_filt$COMMONNAME=="WHALE-SEI")]= "Sei Whale: No Status (SARA) & Endangered (COSEWIC)"
#   wsdb_filt$COMMONNAME[which(wsdb_filt$COMMONNAME=="WHALE-SOWERBY'S BEAKED")]= "Sowerby's Beaked Whale: Special Concern (SARA & COSEWIC)"
# 
#  return(wsdb_filt)
# }

