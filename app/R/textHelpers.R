write_meta <- function(rr, lang) {
  outText <- ""
  if (lang == "en"){
    outText <- c(
      paste("Contact:", rr$contact),
      paste("Last retrieved on:", rr$accessedOn$en),
      paste("Quality Tier:", rr$qualityTier$en),
      paste("Security level:", rr$securityLevel$en), 
      paste("Data use constraints:", rr$constraints$en, "  \n"))
  } else if (lang == "fr") {
    outText <- c(
      paste("Personne-ressource:", rr$contact),
      paste("Consulté le:", rr$accessedOn$fr),
      paste("Niveau de qualité:", rr$qualityTier$fr),
      paste("Niveau de sécurité:", rr$securityLevel$fr), 
      paste("Contraintes d'usage:", rr$constraints$fr, "  \n"))
  }
  writeLines(noquote(outText), sep="  \n")
}
  

