# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))

library(readxl)

scheduleOne <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheet="Schedule1Documents")
nonScheduleOne <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheet="NonSchedule1Documents")
leatherbackTable <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheet="LeatherbackSeaTurtle")
loggerheadTable <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheet="LoggerheadSeaTurtle")
mudPiddockTable <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheet="MudPiddock")

threatsMetadata <- read_google_metadata("threats_rr", isOpenData = TRUE)

save(scheduleOne, nonScheduleOne, leatherbackTable,loggerheadTable, mudPiddockTable, threatsMetadata, file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "Open/threats_rr.RData"))



