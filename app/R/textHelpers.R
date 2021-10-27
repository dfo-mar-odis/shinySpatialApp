write_meta <- function(rr, lang) {
  metadata <- rr$metadata
  outText <- ""
  if (lang == "EN"){
    outText <- c(
      paste("Contact:", metadata$contact),
      ifelse("url" %in% names(metadata), paste("URL:", metadata$url$en), NA),
      paste("Last retrieved on:", metadata$accessedOnStr$en),
      ifelse("searchYears" %in% names(metadata), paste("Search Years:", metadata$searchYears), NA),
      paste("Quality Tier:", metadata$qualityTier$en),
      paste("Security level:", metadata$securityLevel$en), 
      paste("Data use constraints:", metadata$constraints$en),
      ifelse("reference" %in% names(metadata), paste("Reference:", metadata$reference$en), NA)
    )
  } else if (lang == "FR") {
    outText <- c(
      paste("Personne-ressource:", metadata$contact),
      ifelse("url" %in% names(metadata), paste("LIEN:", metadata$url$fr), NA),
      paste("Consulté le:", metadata$accessedOnStr$fr),
      ifelse("searchYears" %in% names(metadata), paste("Année de recherche:", metadata$searchYears), NA),
      paste("Niveau de qualité:", metadata$qualityTier$fr),
      paste("Niveau de sécurité:", metadata$securityLevel$fr), 
      paste("Contraintes d'usage:", metadata$constraints$fr),
      ifelse("reference" %in% names(metadata), paste("Reference:", metadata$reference$fr), NA)
    )
  }
  outText <- outText[!is.na(outText)]
  writeLines(noquote(outText), sep="  \n")
}
  

