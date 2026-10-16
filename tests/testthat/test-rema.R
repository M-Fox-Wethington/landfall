test_that("tile indices follow the PGC naming", {
  # Tile 43_06 covers x from -2,500,000 to -2,400,000 and y from 1,200,000 to
  # 1,300,000, with a 96 m overlap.
  e <- landfall:::rema_tile_extent(43, 6)
  expect_equal(unname(e), c(-2500096, -2399904, 1199904, 1300096))
  box <- sf::st_bbox(c(xmin = -2450000, ymin = 1250000, xmax = -2440000, ymax = 1260000),
                     crs = sf::st_crs(3031))
  t <- rema_tiles_for_bbox(box)
  expect_equal(t$name, "43_06")
  expect_equal(t$file, "43_06_32m_v2.0_dem.tif")
  expect_match(t$url, "REMA/mosaic/v2.0/32m/43_06/43_06_32m_v2.0.tar.gz$")
})

test_that("a box across a tile edge needs both tiles", {
  box <- sf::st_bbox(c(xmin = -2420000, ymin = 1180000, xmax = -2380000, ymax = 1220000),
                     crs = sf::st_crs(3031))
  t <- rema_tiles_for_bbox(box)
  expect_setequal(t$name, c("42_06", "42_07", "43_06", "43_07"))
})

test_that("the tiles on the BRI workstation match the formula", {
  dir <- "G:/BRI-Antarctic-Drone-Processing/figures/cache/rema"
  skip_if_not(dir.exists(dir), "REMA tiles not on this machine")
  files <- list.files(dir, pattern = "^[0-9]+_[0-9]+_32m_v2\\.0_dem\\.tif$", full.names = TRUE)
  skip_if(length(files) == 0)
  for (f in files) {
    rc <- as.integer(strsplit(basename(f), "_")[[1]][1:2])
    e <- as.vector(terra::ext(terra::rast(f)))
    expect_equal(unname(e), unname(landfall:::rema_tile_extent(rc[1], rc[2])), tolerance = 1e-6)
  }
})

test_that("a missing tile stops when no fallback is set", {
  box <- sf::st_bbox(c(xmin = 100000, ymin = 100000, xmax = 110000, ymax = 110000),
                     crs = sf::st_crs(3031))
  paths <- list(rema_dir = withr::local_tempdir(), rema_1km = NULL)
  expect_error(landfall:::rema_dem_for(box, paths), "missing for this window")
})
