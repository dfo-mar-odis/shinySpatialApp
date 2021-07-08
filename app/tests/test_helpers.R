
library(testthat)
library(here)

source("../R/helpers.R", chdir = TRUE)


test_that("check name", {
  expect_equal(check_name("foo"), TRUE)
  expect_equal(check_name(""), FALSE)
})