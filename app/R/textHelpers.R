write_meta <- function(rr) {
  outText <- c(
    paste("Contact: ", rr$contact),
    paste("Last retrieved on: ", rr$accessedOn),
    paste("Quality Tier: ", rr$qualityTier),
    paste("Security level: ", rr$securityLevel), 
    paste("Data use constraints: ", rr$constraints, "  \n"))
  writeLines(noquote(outText), sep="  \n")
}
  

