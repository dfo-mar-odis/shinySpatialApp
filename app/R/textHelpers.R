write_meta <- function(rr, lang) {
  outText <- ""
  if (lang == "EN"){
    outText <- c(
      paste("Contact:", rr$contact),
      ifelse("url" %in% names(rr), paste("URL:", rr$url$en), NA),
      paste("Last retrieved on:", rr$accessedOnStr$en),
      ifelse("searchYears" %in% names(rr), paste("Search Years:", rr$searchYears), NA),
      paste("Quality Tier:", rr$qualityTier$en),
      paste("Security level:", rr$securityLevel$en), 
      paste("Data use constraints:", rr$constraints$en),
      ifelse("reference" %in% names(rr), paste("Reference:", rr$reference$en), NA)
    )
  } else if (lang == "FR") {
    outText <- c(
      paste("Personne-ressource:", rr$contact),
      ifelse("url" %in% names(rr), paste("LIEN:", rr$url$fr), NA),
      paste("Consulté le:", rr$accessedOnStr$fr),
      ifelse("searchYears" %in% names(rr), paste("Année de recherche:", rr$searchYears), NA),
      paste("Niveau de qualité:", rr$qualityTier$fr),
      paste("Niveau de sécurité:", rr$securityLevel$fr), 
      paste("Contraintes d'usage:", rr$constraints$fr),
      ifelse("reference" %in% names(rr), paste("Reference:", rr$reference$fr), NA)
    )
  }
  outText <- outText[!is.na(outText)]
  writeLines(noquote(outText), sep="  \n")
}
  

