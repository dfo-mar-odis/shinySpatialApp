library(testthat)

context("Render Tests")


test_that("render Intro", {
  load_test_data(environment())
  test_out_dir <- here("tests/temp")
  temp_files <- list.files(test_out_dir, include.dirs = T, full.names = T, recursive = T)
  unlink(temp_files, recursive=TRUE)
  out_file <- here("tests/temp/rmd_intro")
 
    chk <- renderReport(
      input = readRDS(here("tests/testthat/input")), # sample input from application, with no sections.
      geoms = studyArea,
      outFileName = out_file,
      dirIn = here("app/Rmd"),
      dirOut = here("tests/temp")
    )
  
  expect_true(file.exists(here("tests/temp/rmd_intro_EN.html")))
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
