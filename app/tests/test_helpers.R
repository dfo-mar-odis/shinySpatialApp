
library(testthat)
library(here)

source("../R/helpers.R", chdir = TRUE)


test_that("check name", {
  expect_equal(check_name("John Doe"), TRUE)
  expect_equal(check_name(""), FALSE)
})