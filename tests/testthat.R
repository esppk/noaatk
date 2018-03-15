library(testthat)
library(noaatk)

test_that("geomTimeline is ggproto obj", {
  expect_is(GeomTimeline, "ggproto" )
  expect_is(GeomTimelinelabel, "ggproto")
})

test_that("geomTimeline is ",{
  expect_is(geom_timeline(), "Layer")
  expect_is(geom_timelinelabel(), "Layer")
})

test_that("poptext label", {
  mat_dat <-   data.frame(LOCATION_NAME_ = "LA",
                          EQ_MAG_MS = "3",
                          DEATHS = "0")
  expect_is(eq_create_label(mat_dat), "character")
})

test_that("leaflet", {
  mat_dat <- data.frame(LOCATION_NAME_ = "LA",
                          EQ_MAG_MS = "3",
                          DEATHS = "0",
                          LONGITUDE = 34.05,
                          LATITUDE= -118.25,
                          DATE = lubridate::ymd("2018-02-12"))
  expect_is(mat_dat %>% eq_map(DATE) %>% class(), "character")
})
