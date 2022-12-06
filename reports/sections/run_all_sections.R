
imSecs <- c(here::here("reports/sections/isleMadame/presence_sections/bnWhale_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/fbWhale_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/greySeal_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/harbourPorpoise_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/harbourSeal_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/imSalmon_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/imSalmonRivers_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/lobster_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/narWhale_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/snowCrab_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/ssClam_preprocessing.R"),
            here::here("reports/sections/isleMadame/presence_sections/tuna_preprocessing.R")
)

openDataSecs <- c(here::here("reports/sections/benthicEffort/benthicEffort_preprocessing.R"),
                  here::here("reports/sections/blueWhaleHab/bwhab_preprocessing.R"),
                  here::here("reports/sections/docks/docks_preprocessing.R"),
                  here::here("reports/sections/ebsa/ebsa_preprocessing.R"),
                  here::here("reports/sections/imAcoustic/imAcoustic_preprocessing.R"),
                    #here::here("reports/sections/janvrinIsland/janvrinIsland_preprocessing.R"),
                  here::here("reports/sections/lobsterEffort/lobsterEffort_preprocessing.R"),
                  here::here("reports/sections/nbw/nbw_preprocessing.R"),
                  here::here("reports/sections/nitroLoad/nitroLoad_preprocessing.R"),
                  here::here("reports/sections/opprrpe/opprrpe_preprocessing.R"),
                  here::here("reports/sections/pasBay/pasBay_preprocessing.R"),
                  #here::here("reports/sections/power/power_preprocessing.R"),
                  here::here("reports/sections/rv/rv_preprocessing.R"),
                  here::here("reports/sections/sdm/sdm_preprocessing.R"),
                  here::here("reports/sections/waste/waste_preprocessing.R"),
                  #here::here("reports/sections/whelk/whelk_preprocessing.R"),
                  imSecs
)

egisSecs <- c(here::here("reports/sections/asf/asf_preprocessing.R"),
              here::here("reports/sections/isdb-marfis/isdbMarfis_preprocessing.R"),
              here::here("reports/sections/permits/permits_preprocessing.R"),
              here::here("reports/sections/rockweed/rockweed_preprocessing.R"),
              here::here("reports/sections/wsdb/wsdb_preprocessing.R")
              )

apiSecs <- c( here::here("reports/sections/obis/obis_preprocessing.R"),
              here::here("reports/sections/obis/gbif_preprocessing.R"),
              here::here("reports/sections/sarsearch/sarsearch_preprocessing.R")
              )

inDriveSecs <- c(here::here("reports/sections/commonSections/commonData_preprocessing.R"),
                 here::here("reports/sections/conservationSites/conservationSites_preprocessing.R"),
                 here::here("reports/sections/crithab/crithab_preprocessing.R"),
                 here::here("reports/sections/electrofishing/ef_preprocessing.R"),
                 here::here("reports/sections/ilts/ilts_preprocessing.R"),
                 here::here("reports/sections/narwc/narwc_preprocessing.R"),
                 here::here("reports/sections/ocearch/ocearch_preprocessing.R"),
                 here::here("reports/sections/offshoreScallop/offshoreScallop_preprocessing.R"),
                 #here::here("reports/sections/sardist/sardist_preprocessing.R"),
                 here::here("reports/sections/sturg/sturg_preprocessing.R"),
                 #here::here("reports/sections/threats/ThreatsPreprocessing.R"),
                 #here::here("reports/sections/whitehead/whitehead_preprocessing.R")
)

allSecs <- c(openDataSecs, egisSecs, apiSecs, inDriveSecs)

source_preprocessing_script <- function(scriptPath) {
  tryCatch(
    expr = {
      suppressWarnings(source(scriptPath, encoding="utf-8"))
    },
    error = function(e){ 
      print(paste("error with:", scriptPath))
      print(e)
    }
  )
}


# create global env:
source(here::here("config.R"))
globalControlEnv$saveToRemote <- FALSE
globalControlEnv$updateGeoms <- TRUE

if (FALSE) {
  # need utf-8 for accents
  lapply(openDataSecs, source_preprocessing_script, encording = "utf-8")
  lapply(egisSecs, source_preprocessing_script)
  lapply(apiSecs, source_preprocessing_script)
  lapply(inDriveSecs, source_preprocessing_script)  
  lapply(imSecs, source_preprocessing_script)  
  lapply(allSecs, source_preprocessing_script)  

  
  
  # Parallization WHOOOOO!
  start <- proc.time()
  cl <- parallel::makeCluster(6, outfile = here::here("reports/temp/parallel.log"))
  parallel::parSapply(cl , allSecs , source_preprocessing_script)
  parallel::stopCluster(cl)
  print(proc.time() - start)
  
}  
