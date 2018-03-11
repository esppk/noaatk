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
  expect_is(eq_create_label(mat_dat), "character")
})

test_that("leaflet", {
  expect_is(map_dat %>% eq_map(DATE) %>% class(), "leaflet")
})
