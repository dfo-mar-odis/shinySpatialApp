library(testthat)
library(here)


# ---------------------PREPARE DATA--------------------------
source(here("app/tests/helper_functions_for_tests.R"))

# prepare test environment, similar to data loading in into_EN.rmd
load_test_data()

# select random sample area from test sample areas
study_area_opts <- list.files(here("app/studyAreaTest/"), pattern = "*.geojson", full.names = TRUE)
studyArea <- st_read(sample(study_area_opts, 1))

# prepped_data is a list of needed variables, eg site, studyBox_geom, etc.
prepped_data <- prep_test_data(studyArea)
list2env(prepped_data, env=globalenv())


# ----------------------------TESTS---------------------------------------
# tests.  After preparing the environment, any of the following can be run individually:
test_that("render Intro", {
  # will need to set valid options in the RMD for the intro
  success <- render_check_delete(here("app/Rmd/intro_EN.Rmd"))
  expect_true(success)
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
