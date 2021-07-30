
# ---------------------PREPARE DATA--------------------------
source(here::here("tests/testthat/help_functions_for_tests.R"))
message("Generate new test data")
catch_all_output({
  # generate a new testData file
  source(here::here("dataprocessing/MakeTestData.R"))
  gen_all_test_data()
})
message("Test data generation complete")