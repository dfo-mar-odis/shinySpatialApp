library(testthat)
library(here)


# ---------------------PREPARE DATA--------------------------
source(here("app/tests/helper_functions_for_tests.R"))

# prepare test environment, similar to data loading in into_EN.rmd
load_test_data()

# select random sample area from test sample areas
studyAreaOpts <- here("app/studyAreaTest/", c("geoms_slc_coastal_test.geojson", 
                                              "geoms_slc_no_land.geojson", 
                                              "geoms_slc_coastal_test.geojson"))



studyArea <- st_read(sample(studyAreaOpts, 1))

# prepped_data is a list of needed variables, eg site, studyBox_geom, etc.
preppedData <- prep_test_data(studyArea)
list2env(preppedData, env=globalenv())

# ----------------------------TESTS---------------------------------------
# tests.  After preparing the environment, any of the following can be run individually:
test_that("render Intro", {
  test_out_dir = here("app/tests/temp")
  temp_files <- list.files(test_out_dir, include.dirs = T, full.names = T, recursive = T)
  unlink(temp_files, recursive=TRUE)
  out_file = here("app/tests/temp/rmd_intro")
  chk <- renderReport(
    input = readRDS(here("app/tests/input")), # sample input from application, with no sections.
    geoms = studyArea,
    fl = out_file,
    dir_in = here("app/Rmd"),
    dir_out = here("app/tests/temp")
  )
  expect_true(file.exists(here("app/tests/temp/rmd_intro_EN.html")))
})

test_that("render SARA", {
  success <- render_check_delete(here("app/Rmd/report_SARA_EN.Rmd"))
  expect_true(success)
})

test_that("render Ceteceans", {
  success <- render_check_delete(here("app/Rmd/report_cetaceans_EN.Rmd"))
  expect_true(success)
})

test_that("render Fish", {
  success <- render_check_delete(here("app/Rmd/report_fish_EN.Rmd"))
  expect_true(success)
})

test_that("render habitat", {
  success <- render_check_delete(here("app/Rmd/report_habitat_EN.Rmd"))
  expect_true(success)
})

test_that("render planning", {
  success <- render_check_delete(here("app/Rmd/report_planning_EN.Rmd"))
  expect_true(success)
})
