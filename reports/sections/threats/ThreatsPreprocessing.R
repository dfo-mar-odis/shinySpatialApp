source(here::here("config.R"))
library(readxl) # ask Quentin: where else do I need to add this?

scheduleOne <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheet="Schedule1Documents")
nonScheduleOne <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheet="NonSchedule1Documents")
leatherbackTable <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheet="LeatherbackSeaTurtle")
loggerheadTable <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheet="LoggerheadSeaTurtle")
mudPiddockTable <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheet="MudPiddock")

save(scheduleOne, nonScheduleOne, leatherbackTable,loggerheadTable, mudPiddockTable, file = file.path(localFileSavePath, "Open/threats_rr.RData"))
