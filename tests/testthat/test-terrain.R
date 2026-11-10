test_that("every color in the hypsometric ramp can be reached", {
  br <- landfall:::HYPSO_BREAKS
  pal <- landfall:::HYPSO_PALETTE
  expect_equal(length(br), length(pal))
  heights <- c(0, 100, 500, 1000, 2399, 2400, 5000)
  idx <- pmax(1L, pmin(length(pal), findInterval(heights, br)))
  expect_equal(min(idx), 1L)
  expect_equal(max(idx), length(pal))
  # all.inside = TRUE coerces the index into 1 to length(br) - 1, so with one
  # color per break the last color could never be drawn. This records why the
  # clamp replaced it.
  expect_lt(max(findInterval(heights, br, all.inside = TRUE)), length(pal))
})

test_that("the ramp is only indexed above the floor the relief uses", {
  # Land that REMA reads below the corrected sea surface is raised to the floor
  # rather than dropped, and the floor has to fall in the first band.
  br <- landfall:::HYPSO_BREAKS
  expect_gt(landfall:::TERRAIN_FLOOR_M, 0)
  expect_equal(findInterval(landfall:::TERRAIN_FLOOR_M, br), 1L)
})

fake_coast <- function(source = "high-resolution coastline v7.12") {
  list(land = NULL, shelves = NULL, source = source, files = character(0))
}

test_that("the relief signature changes with every input it is built from", {
  w <- site_window("NEKO", radius_km = 25)
  cst <- fake_coast()
  sig <- function(window = w, coast = cst, grid_res = 50, z = 4, floor = 0.55,
                  collar = 60, src = "32 m", bathy = NA_character_) {
    landfall:::terrain_signature(window, coast, grid_res, z, floor, collar, src,
                                 character(0), bathy)
  }
  base <- sig()
  expect_false(identical(sig(grid_res = 25), base))
  expect_false(identical(sig(z = 3), base))
  expect_false(identical(sig(floor = 0.40), base))
  expect_false(identical(sig(collar = 120), base))
  expect_false(identical(sig(src = "1 km"), base))
  expect_false(identical(sig(coast = fake_coast("high-resolution coastline v7.10")), base))
  expect_false(identical(sig(window = site_window("NEKO", radius_km = 10)), base))
  expect_false(identical(sig(window = site_window("NEKO", radius_km = 25, lat_ts = -60)),
                         base))
  expect_false(identical(sig(window = site_window("NEKO", radius_km = 25, pad_m = 9000)),
                         base))
  # The bed model is one of the inputs, because the sea floor is colored from
  # it. A relief built when there was none on disk must not be reused once one
  # is there, which is the fault this project has now found in four caches.
  bed <- withr::local_tempfile(fileext = ".nc")
  writeLines("stands in for a bed model", bed)
  expect_false(identical(sig(bathy = bed), base))
  # The same inputs twice give the same signature, so a matching cache is
  # reused rather than rebuilt on every call.
  expect_identical(sig(), base)
})

test_that("two different places sharing a cache file name have different signatures", {
  # This is the collision the file name alone could not catch: a point of your
  # own labelled NEKO, at the same radius and meridian as Neko Harbor, writes
  # to the same cache file.
  harbor <- site_window("NEKO", radius_km = 25)
  ridge <- site_window(site_point("Neko Ridge", lon = harbor$site$lon, lat = -64.60),
                       radius_km = 25, lon_0 = harbor$lon_0)
  expect_equal(landfall:::window_stem(ridge), landfall:::window_stem(harbor))
  cst <- fake_coast()
  expect_false(identical(
    landfall:::terrain_signature(ridge, cst, 50, 4, 0.55, 60, "32 m", character(0),
                                 NA_character_),
    landfall:::terrain_signature(harbor, cst, 50, 4, 0.55, 60, "32 m", character(0),
                                 NA_character_)))
})

test_that("a file's identity carries its size and when it was written", {
  f <- withr::local_tempfile()
  writeLines("one", f)
  first <- landfall:::file_identity(f)
  expect_match(first, basename(f), fixed = TRUE)
  writeLines(c("one", "two", "three"), f)
  expect_false(identical(landfall:::file_identity(f), first))
  expect_equal(landfall:::file_identity(character(0)), "none")
  expect_equal(landfall:::file_identity(file.path(tempdir(), "not-here-at-all")), "none")
})

test_that("clearing the terrain cache reports what it removed", {
  dir <- withr::local_tempdir()
  paths <- list(terrain_cache = dir)
  file.create(file.path(dir, c("neko_r25_lon-62.5_relief_rgb.tif",
                              "neko_r25_lon-62.5_relief_signature.txt",
                              "cuve_r25_lon-62.6_relief_rgb.tif")))
  expect_message(clear_terrain_cache("NEKO", paths = paths), "Removed 2")
  expect_true(file.exists(file.path(dir, "cuve_r25_lon-62.6_relief_rgb.tif")))
  expect_message(clear_terrain_cache(paths = paths), "Removed 1")
  expect_equal(length(list.files(dir)), 0)
})

test_that("the bed model is cropped in its own grid and resampled onto the window", {
  w <- site_window("NEKO", radius_km = 25)
  # A stand-in for IBCSO: the whole Southern Ocean at a coarse step, in the
  # projection IBCSO v2 is stored in, which is not the window's own. Depth
  # falls away from the pole so the values can be told apart after projection.
  ibcso <- terra::rast(xmin = -4.8e6, xmax = 4.8e6, ymin = -4.8e6, ymax = 4.8e6,
                       resolution = 2e4,
                       crs = "+proj=stere +lat_0=-90 +lat_ts=-65 +lon_0=0 +datum=WGS84 +units=m")
  xy <- terra::xyFromCell(ibcso, seq_len(terra::ncell(ibcso)))
  terra::values(ibcso) <- -sqrt(xy[, 1]^2 + xy[, 2]^2) / 1000
  f <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(ibcso, f, overwrite = TRUE)

  template <- terra::rast(xmin = w$xlim[1] - w$pad_m, xmax = w$xlim[2] + w$pad_m,
                          ymin = w$ylim[1] - w$pad_m, ymax = w$ylim[2] + w$pad_m,
                          resolution = 200, crs = w$crs$wkt)
  got <- landfall:::window_bathymetry(w, template, list(bathymetry = f))
  expect_s4_class(got, "SpatRaster")
  # It comes back on the window's grid, not the bed model's, so the water can
  # be colored cell for cell against the relief.
  expect_equal(as.vector(terra::ext(got)), as.vector(terra::ext(template)))
  expect_equal(terra::res(got), terra::res(template))
  # No rim of nothing around the window: the crop takes a margin counted in
  # the bed model's own cells, which here are 20 km wide.
  expect_true(all(is.finite(terra::values(got, mat = FALSE))))

  # No setting, or a path to nothing, and the map draws the sea flat instead
  # of stopping.
  expect_null(landfall:::window_bathymetry(w, template, list()))
  expect_null(landfall:::window_bathymetry(w, template, list(bathymetry = "no_such.nc")))
})
