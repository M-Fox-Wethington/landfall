# A small GeoPackage in the style of the ADD high-resolution coastline: two
# polygons with a `surface` column, in EPSG:3031, around a point that a window
# can be built on.
write_fake_coast <- function(path, surfaces = c("land", "ice shelf")) {
  ctr <- sf::st_transform(sf::st_sfc(sf::st_point(c(-62.5327, -64.8383)), crs = 4326), 3031)
  xy <- sf::st_coordinates(ctr)
  sq <- function(dx, dy, half = 8000) {
    sf::st_polygon(list(rbind(c(xy[1] + dx - half, xy[2] + dy - half),
                              c(xy[1] + dx + half, xy[2] + dy - half),
                              c(xy[1] + dx + half, xy[2] + dy + half),
                              c(xy[1] + dx - half, xy[2] + dy + half),
                              c(xy[1] + dx - half, xy[2] + dy - half))))
  }
  x <- sf::st_sf(surface = surfaces,
                 geom = sf::st_sfc(sq(-10000, 0), sq(10000, 0), crs = 3031))
  sf::st_write(x, path, layer = "coast", quiet = TRUE)
  path
}

# The same two polygons stored in EPSG:4326, to check that the spatial filter
# is written in the coordinates of the file it is applied to.
write_fake_coast_4326 <- function(path, surfaces = c("land", "ice shelf")) {
  src <- tempfile(fileext = ".gpkg")
  on.exit(unlink(src), add = TRUE)
  x <- sf::st_read(write_fake_coast(src, surfaces), quiet = TRUE)
  sf::st_write(sf::st_transform(x, 4326), path, layer = "coast", quiet = TRUE)
  path
}

test_that("the high-resolution coastline is split into land and shelves", {
  f <- write_fake_coast(withr::local_tempfile(fileext = ".gpkg"))
  w <- site_window("NEKO", radius_km = 25)
  coast <- load_coast(w, paths = list(coastline_hr = f))
  expect_match(coast$source, "high-resolution")
  expect_equal(nrow(coast$land), 1)
  expect_equal(nrow(coast$shelves), 1)
  expect_true(sf::st_crs(coast$land) == w$crs)
  expect_equal(coast$files, f)
})

test_that("a layer stored in another CRS is still filtered to the window", {
  # The filter geometry used to be written in EPSG:3031 whatever the file held,
  # which would return the wrong features, or none, from a layer in any other
  # coordinate system.
  f <- write_fake_coast_4326(withr::local_tempfile(fileext = ".gpkg"))
  w <- site_window("NEKO", radius_km = 25)
  expect_true(sf::st_crs(landfall:::layer_crs(f, "coast")) == sf::st_crs(4326))
  coast <- load_coast(w, paths = list(coastline_hr = f))
  expect_equal(nrow(coast$land), 1)
  expect_equal(nrow(coast$shelves), 1)
  expect_true(sf::st_crs(coast$land) == w$crs)
})

test_that("the coastline caption reads its version from the file that was loaded", {
  w <- site_window("NEKO", radius_km = 25)
  versioned <- withr::local_tempfile(pattern = "add_coastline_high_res_polygon_v7_12_",
                                     fileext = ".gpkg")
  write_fake_coast(versioned)
  expect_equal(load_coast(w, paths = list(coastline_hr = versioned))$source,
               "high-resolution coastline v7.12")
  plain <- write_fake_coast(withr::local_tempfile(fileext = ".gpkg"))
  expect_match(load_coast(w, paths = list(coastline_hr = plain))$source,
               "no version in the file name", fixed = TRUE)
})

test_that("rumples and ice tongues go to the right side", {
  f <- write_fake_coast(withr::local_tempfile(fileext = ".gpkg"), c("rumple", "ice tongue"))
  w <- site_window("NEKO", radius_km = 25)
  coast <- load_coast(w, paths = list(coastline_hr = f))
  expect_equal(nrow(coast$land), 1)
  expect_equal(nrow(coast$shelves), 1)
})

test_that("a window with nothing in it reads as empty layers", {
  f <- write_fake_coast(withr::local_tempfile(fileext = ".gpkg"))
  w <- site_window("HALF", radius_km = 5)
  coast <- load_coast(w, paths = list(coastline_hr = f))
  expect_equal(nrow(coast$land), 0)
  expect_equal(nrow(coast$shelves), 0)
})

test_that("missing coastline files are reported", {
  w <- site_window("NEKO", radius_km = 5)
  expect_error(load_coast(w, paths = list(coastline = "nope.shp")), "coastline layer")
  expect_null(load_contours(w, paths = list()))
  expect_null(load_rock(w, paths = list()))
})
