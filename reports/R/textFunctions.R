
######### WRITE META ##############
# Function writes a blurb based on the metadata of an rr object.
# Typically used at the start of each subsection in the reports
# Inputs:
# rr: An rr object
# lang: EN or FR used to the set the language of the blurb.
write_meta <- function(rr, lang) {
  metadata <- rr$metadata
  outText <- ""
  if (lang == "EN"){
    outText <- c(
      paste("Contact:", lang_check(metadata$contact, lang)),
      ifelse(!is.na(metadata$url$en), paste("URL:", metadata$url$en), NA),
      paste("Last retrieved on:", metadata$accessedOnStr$en),
      ifelse(!is.na(metadata$searchYears), paste("Search Year:", metadata$searchYears), NA),
      paste("Quality tier:", metadata$qualityTier$en),
      paste("Security level:", metadata$securityLevel$en), 
      paste("Data use constraints:", metadata$constraints$en),
      ifelse(!is.na(metadata$reference$en), paste("Reference:", metadata$reference$en), NA),
      ifelse(!is.na(metadata$pipelinePath), paste("Pipeline Path:", metadata$pipelinePath), NA)
    )
  } else if (lang == "FR") {
    outText <- c(
      paste("Personne-ressource:",lang_check(metadata$contact, lang)),
      ifelse(!is.na(metadata$url$fr), paste("LIEN:", metadata$url$fr), NA),
      paste("Consulté le:", metadata$accessedOnStr$fr),
      ifelse(!is.na(metadata$searchYears), paste("Année de recherche:", metadata$searchYears), NA),
      paste("Niveau de qualité:", metadata$qualityTier$fr),
      paste("Niveau de sécurité:", metadata$securityLevel$fr), 
      paste("Contraintes d'usage:", metadata$constraints$fr),
      ifelse(!is.na(metadata$reference$fr), paste("Reference:", metadata$reference$fr), NA),
      ifelse(!is.na(metadata$pipelinePath), paste("Pipeline de données:", metadata$pipelinePath), NA)
    )
  }
  outText <- outText[!is.na(outText)]
  writeLines(noquote(outText), sep="  \n")
}


lang_check <- function(metadata_arg, lang){
  if (lang == "FR") {
    return(ifelse(rlang::has_name(metadata_arg, "fr"), metadata_arg$fr, metadata_arg))
  } else if (lang == "EN") {
    return(ifelse(rlang::has_name(metadata_arg, "en"), metadata_arg$en, metadata_arg))
  }
}


######## WRITE CAPTION BLURB ############  
# Used to output a blurb containing the information used in figure and table captions.
# Inputs:
# rr: An rr object
# lang: EN or FR used to the set the language of the blurb.
write_caption_blurb <- function(rr, lang, constraints=FALSE) {
  metadata <- rr$metadata
  outText <- ""
  if (lang == "EN"){
    outText <- paste0("Quality tier: ", metadata$qualityTier$en, ". ", 
                     "Security level: ", metadata$securityLevel$en, ". ",
                     ifelse(constraints, paste0("Constraints:", metadata$constraints$en, ". "), "")
                     ) 
    
  } else if (lang == "FR") {
    outText <- paste0("Niveau de qualité:", metadata$qualityTier$fr, ". ", 
                     "Niveau de sécurité:", metadata$securityLevel$fr, ". ", 
                     ifelse(constraints, paste0("Constraints:", metadata$constraints$fr, ". "), "")
                     ) 
  }
  return(outText)
}


# --------------lang_list-----------------
# Helper function that converts a string into a bilinugual list
lang_list <- function(inValue) {
  return(list("en" = inValue, "fr" = inValue))
}

