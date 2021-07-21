
library(testthat)
library(here)

source(here("app/R/helpers.R"))


test_that("check name", {
  expect_equal(check_name("John Doe"), TRUE)
  expect_equal(check_name(""), FALSE)
})