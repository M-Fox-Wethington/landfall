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

test_that("the high-resolution coastline is split into land and shelves", {
  f <- write_fake_coast(withr::local_tempfile(fileext = ".gpkg"))
  w <- site_window("NEKO", radius_km = 25)
  coast <- load_coast(w, paths = list(coastline_hr = f))
  expect_match(coast$source, "high-resolution")
  expect_equal(nrow(coast$land), 1)
  expect_equal(nrow(coast$shelves), 1)
  expect_equal(as.character(sf::st_crs(coast$land)$wkt == w$crs$wkt), "TRUE")
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
