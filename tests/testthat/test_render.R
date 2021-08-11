library(testthat)

context("Render Tests")


test_that("render Intro", {
  list2env(load_test_data(), envir = environment())
  test_out_dir <- here::here("tests/temp")
  temp_files <- list.files(test_out_dir, include.dirs = T, full.names = T, recursive = T)
  unlink(temp_files, recursive=TRUE)
  out_file <- here::here("tests/temp/rmd_intro")
 
  chk <- renderReport(
    # use sample input from application, with no sections:
    input = readRDS(here::here("tests/testthat/input")),
    geoms = studyArea,
    outFileName = out_file,
    dirIn = here::here("app/Rmd"),
    dirOut = here::here("tests/temp")
  )
  
  expect_true(file.exists(here::here("tests/temp/rmd_intro_EN.html")))
})

test_that("render SARA", {
  success <- render_check_delete(here::here("app/Rmd/report_SARA_EN.Rmd"))
  expect_true(success)
})

test_that("render Ceteceans", {
  success <- render_check_delete(here::here("app/Rmd/report_cetaceans_EN.Rmd"))
  expect_true(success)
})

test_that("render Fish", {
  success <- render_check_delete(here::here("app/Rmd/report_fish_EN.Rmd"))
  expect_true(success)
})

test_that("render habitat", {
  success <- render_check_delete(here::here("app/Rmd/report_habitat_EN.Rmd"))
  expect_true(success)
})

test_that("render planning", {
  success <- render_check_delete(here::here("app/Rmd/report_planning_EN.Rmd"))
  expect_true(success)
})
