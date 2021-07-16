library(testthat)
library(here)

#set wd:
setwd(here("./app"))

source("./R/renderReport.R")

input <- readRDS("./tests/input.RDS")

geoms <- readRDS("./tests/geom.RDS")


test_that("check render", {
  chk <- renderReport(
    input = input,
    geoms = geoms,
    fl = input$report_name
  )
  expect_true(chk$ok)
})