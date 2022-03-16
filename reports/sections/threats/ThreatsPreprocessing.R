source(here::here("config.R"))
nonShecduleOne <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheetName="")
leatherbackTable <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheetName="")
nonScheduleOne <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheetName="")
nonSchecduleOne <- read_excel(file.path(fileLoadPath, "NaturalResources/Species/Threats/ThreatData.xlsx"), sheetName="")

save(c(nonScheduleOne, Leatherback), file = file.path(localFileSavePath, "Open/threats_rr.RData"))
