write_meta <- function(rr, lang) {
  outText <- ""
  if (lang == "EN"){
    outText <- c(
      paste("Contact:", rr$contact),
      paste("Last retrieved on:", rr$accessedOnStr$en),
      paste("Quality Tier:", rr$qualityTier$en),
      paste("Security level:", rr$securityLevel$en), 
      paste("Data use constraints:", rr$constraints$en))
  } else if (lang == "FR") {
    outText <- c(
      paste("Personne-ressource:", rr$contact),
      paste("Consulté le:", rr$accessedOnStr$fr),
      paste("Niveau de qualité:", rr$qualityTier$fr),
      paste("Niveau de sécurité:", rr$securityLevel$fr), 
      paste("Contraintes d'usage:", rr$constraints$fr))
  }
  writeLines(noquote(outText), sep="  \n")
}
  

