test_that("a path can be cleared and not only replaced", {
  withr::local_options(landfall.paths = list())
  landfall_paths(output = withr::local_tempdir())
  expect_false(is.null(landfall_paths()$output))
  # An omitted argument leaves a setting alone, so clearing one needs a value
  # of its own.
  landfall_paths(cache = withr::local_tempdir())
  expect_false(is.null(landfall_paths()$output))
  landfall_paths(output = NA)
  expect_null(landfall_paths()$output)
})

test_that("every setting can be forgotten at once", {
  withr::local_options(landfall.paths = list())
  landfall_paths(spatial = "X:/nowhere", output = "X:/also-nowhere")
  expect_false(is.null(landfall_paths()$spatial))
  expect_false(is.null(landfall_paths()$coastline_hr))
  landfall_paths(reset = TRUE)
  expect_null(landfall_paths()$spatial)
  expect_null(landfall_paths()$output)
  expect_null(landfall_paths()$coastline_hr)
})

test_that("the ADD layers follow the spatial folder unless set on their own", {
  withr::local_options(landfall.paths = list())
  landfall_paths(spatial = "X:/spatial", coastline_hr = "X:/elsewhere/coast.gpkg")
  p <- landfall_paths()
  expect_match(p$coastline_hr, "elsewhere")
  expect_match(p$contours, "spatial")
})

test_that("a missing layer is reported with the setting to change", {
  expect_error(landfall:::require_path(list(), "coastline", "The ADD coastline layer"),
               "landfall_paths\\(\\)\\$coastline is not set")
  expect_error(landfall:::require_path(list(coastline = "nope.shp"), "coastline", "X"),
               "not found at")
  expect_false(landfall:::has_path(list(), "coastline"))
  expect_false(landfall:::has_path(list(coastline = "nope.shp"), "coastline"))
})
