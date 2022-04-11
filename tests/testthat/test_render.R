library(testthat)

context("Render Tests")

# create random study area:
list2env(load_test_data(), envir = environment())
regionBbox <- sf::st_bbox(region)
xmin <-  runif(1, min=regionBbox$xmin, max=regionBbox$xmax)
ymin <-  runif(1, min=regionBbox$ymin, max=regionBbox$ymax)
xmax <- xmin + 1
ymax <- ymin + 1
polyMatrix <- matrix(c(xmin,ymin,xmax,ymin,xmax,ymax,xmin,ymax,xmin,ymin),ncol=2, byrow=TRUE)
testStudyArea <- st_sfc(st_polygon(list(polyMatrix)))
outputGeojson <- here::here("app/output/geoms_slc.geojson")
unlink(outputGeojson)
sf::st_write(testStudyArea, outputGeojson, append = FALSE)



test_that("render Intro", {
  list2env(load_test_data(), envir = environment())
  test_out_dir <- here::here("tests/temp")
  temp_files <- list.files(test_out_dir, include.dirs = T, full.names = T, recursive = T)
  unlink(temp_files, recursive = TRUE)
  out_file <- here::here("tests/temp/rmd_intro")
  catch_all_output({
    chk <- renderReport(
      # use sample input from application, with no sections:
      input = readRDS(here::here("tests/testthat/input")),
      geoms = studyArea,
      outFileName = out_file,
      dirIn = here::here("app/Rmd"),
      dirOut = here::here("tests/temp")
    )
  })
  expect_true(file.exists(here::here("tests/temp/rmd_intro_EN.html")))
})

enFiles <-list.files(path=here::here("sections/"), pattern=c("*_en.Rmd"),
                     recursive=TRUE, full.names = TRUE)

frFiles <-list.files(path=here::here("sections/"), pattern=c("*_fr.Rmd"),
                     recursive=TRUE, full.names = TRUE)

lapply(enFiles, test_render)

