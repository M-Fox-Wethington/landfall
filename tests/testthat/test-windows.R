test_that("a window is a square of the radius around the site", {
  w <- site_window("NEKO", radius_km = 25)
  expect_s3_class(w, "landfall_window")
  expect_equal(diff(w$xlim), 50000)
  expect_equal(diff(w$ylim), 50000)
  expect_equal(mean(w$xlim), unname(w$center["x"]))
  expect_equal(w$lon_0, site_lookup("NEKO")$lon)
  # In a projection turned to the site's meridian the site sits at x = 0.
  expect_equal(unname(w$center["x"]), 0, tolerance = 1e-6)
  expect_output(print(w), "25 km around")
})

test_that("the padded box in EPSG:3031 holds the frame", {
  w <- site_window("NEKO", radius_km = 10, pad_m = 2000)
  corners <- sf::st_as_sfc(w$bbox) |> sf::st_cast("POINT") |> sf::st_transform(3031) |>
    sf::st_coordinates()
  expect_true(all(corners[, 1] >= w$bbox_3031[["xmin"]] & corners[, 1] <= w$bbox_3031[["xmax"]]))
  expect_true(all(corners[, 2] >= w$bbox_3031[["ymin"]] & corners[, 2] <= w$bbox_3031[["ymax"]]))
  expect_match(w$wkt_3031, "^POLYGON")
})

test_that("a different central meridian can be asked for", {
  w <- site_window("NEKO", radius_km = 25, lon_0 = -60)
  expect_equal(w$lon_0, -60)
  expect_true(abs(w$center["x"]) > 1000)
  expect_equal(landfall:::window_stem(w), "neko_r25_lon-60")
})
