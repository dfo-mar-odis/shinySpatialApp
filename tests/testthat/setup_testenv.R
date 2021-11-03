
# ---------------------PREPARE DATA--------------------------
# save current state and start from clean slate:
message("Saving Environment")
save.image(file=here::here('tests/preTestEnv.RData'))
rm(list=ls(envir = globalenv()), envir = globalenv())

source(here::here("tests/testthat/help_functions_for_tests.R"))
message("Generate and load test data set")
catch_all_output({
  lapply(list.files(here::here("app/R"), pattern = ".[Rr]$",
                    full.names = TRUE), source, local=globalenv())
})

message("Test data generation complete")

