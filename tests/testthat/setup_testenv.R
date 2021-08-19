
library(here)
library(testthat)

# ---------------------PREPARE DATA--------------------------
# save current state and start from clean slate:
message("Saving Environment")
save.image(file=here('tests/preTestEnv.RData'))
rm(list=ls(envir = globalenv()), envir = globalenv())

source(here("tests/testthat/help_functions_for_tests.R"))
source(here("app/R/helpers.R")) #load all libraries
message("Generate new test data set")
catch_all_output({
  # generate a new testData file
  source(here("dataprocessing/MakeTestData.R"))
  gen_all_test_data()
  
  load(here("app/data/testData.RData"), envir = globalenv())
  lapply(list.files(here("app/R"), pattern = ".[Rr]$",
                    full.names = TRUE), source, local=globalenv())
})

message("Test data generation complete")