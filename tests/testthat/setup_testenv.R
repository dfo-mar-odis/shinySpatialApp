
# ---------------------PREPARE DATA--------------------------
# save current state and start from clean slate:
message("Saving Environment")
save.image(file=here::here('tests/preTestEnv.RData'))
rm(list=ls(envir = globalenv()), envir = globalenv())

source(here::here("tests/testthat/help_functions_for_tests.R"))
message("Generate new test data set")
catch_all_output({
  # generate a new testData file
  source(here::here("dataprocessing/MakeTestData.R"))
  gen_all_test_data()
  
  load(here::here("app/data/testData.RData"), envir = globalenv())
  lapply(list.files(here::here("app/R"), pattern = ".[Rr]$",
                    full.names = TRUE), source, local=globalenv())
})

message("Test data generation complete")