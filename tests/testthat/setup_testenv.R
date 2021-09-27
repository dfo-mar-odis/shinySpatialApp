
library(here)
library(testthat)

# ---------------------PREPARE DATA--------------------------
# save current state and start from clean slate:
message("Saving Environment")
save.image(file=here('tests/preTestEnv.RData'))
rm(list=ls(envir = globalenv()), envir = globalenv())

<<<<<<< HEAD
source(here("tests/testthat/help_functions_for_tests.R"))
source(here("app/R/helpers.R")) #load all libraries
message("Generate new test data set")
catch_all_output({
  # generate a new testData file
  source(here("dataprocessing/MakeTestData.R"))
  gen_all_test_data()
=======
source(here::here("tests/testthat/help_functions_for_tests.R"))
message("Generate and load test data set")
catch_all_output({
  
  dataFilesExist <- all(file.exists(c(here::here("app/data/OpenData.RData"),
                                  here::here("app/data/OpenData_sardist.RData"),
                                  here::here("app/data/SecureData.RData"))))
  if (dataFilesExist) {
    source(here::here("dataprocessing/MakeTestData.R"))
    gen_all_test_data()
  }  
>>>>>>> 0f93bd195032197ae79dda484dc6a76e3fc1f0f6
  
  load(here("app/data/testData.RData"), envir = globalenv())
  lapply(list.files(here("app/R"), pattern = ".[Rr]$",
                    full.names = TRUE), source, local=globalenv())
})

message("Test data generation complete")

