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

test_that("a default layer is the first candidate that is on disk", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "add_v7_11"), recursive = TRUE)
  here <- file.path(dir, "add_v7_11", "add_rock_outcrop_high_res_polygon_v7_11.gpkg")
  writeLines("x", here)
  withr::local_options(landfall.paths = list())
  landfall_paths(spatial = dir)
  # The release the package used to name is not here and this one is, so the
  # default follows the disk rather than a release pinned years ago.
  expect_equal(normalizePath(landfall_paths()$rock, winslash = "/"),
               normalizePath(here, winslash = "/"))
})

test_that("a default names a path to look at even when no candidate is on disk", {
  withr::local_options(landfall.paths = list())
  landfall_paths(spatial = "X:/spatial")
  expect_match(landfall_paths()$rock, "^X:/spatial/")
  expect_match(landfall_paths()$bathymetry, "IBCSO", fixed = TRUE)
  expect_false(landfall:::has_path(landfall_paths(), "rock"))
})
