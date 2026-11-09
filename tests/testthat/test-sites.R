test_that("a MAPPPD code looks up its site", {
  s <- site_lookup("neko")
  expect_s3_class(s, "landfall_site")
  expect_equal(s$code, "NEKO")
  expect_equal(s$mapppd_id, "NEKO")
  expect_match(s$name, "Neko Harbor")
  expect_equal(s$lat, -64.8383, tolerance = 1e-4)
  expect_equal(s$lon, -62.5327, tolerance = 1e-4)
  expect_output(print(s), "NEKO")
  expect_error(site_lookup("NEKX"), "No MAPPPD site")
})

test_that("a named point makes a site with no MAPPPD identity", {
  s <- site_point("Stonington Island", lon = -67.0, lat = -68.18)
  expect_equal(s$code, "STON")
  expect_null(s$mapppd_id)
  expect_equal(s$source, "user")
  expect_output(print(s), "Not a MAPPPD site")
  expect_error(site_point("Too far north", lon = 0, lat = -40))
})

test_that("a point whose code collides with a MAPPPD site inherits no counts", {
  # The default code is the first four letters of the name, so this point is
  # labelled NEKO. Neko Harbor holds dozens of counts and none of them belong
  # to a ridge somebody named.
  ridge <- site_point("Neko Ridge", lon = -62.50, lat = -64.80)
  expect_equal(ridge$code, "NEKO")
  expect_null(ridge$mapppd_id)
  expect_equal(nrow(colony_record(ridge)), 0)
  expect_gt(nrow(colony_record("NEKO")), 0)
  # The columns are still there, so anything reading the record keeps working.
  expect_true(all(c("species_id", "season", "type", "count") %in% names(colony_record(ridge))))
})

test_that("a point can be linked to a MAPPPD site on purpose", {
  p <- site_point("Neko Harbor beach", lon = -62.53, lat = -64.84, mapppd_id = "neko")
  expect_equal(p$mapppd_id, "NEKO")
  expect_equal(nrow(colony_record(p)), nrow(colony_record("NEKO")))
  expect_error(site_point("Somewhere", lon = -62, lat = -64, mapppd_id = "NEKX"),
               "cannot stand for it")
})

test_that("as_site accepts codes, sites and lists", {
  expect_equal(landfall:::as_site("NEKO")$code, "NEKO")
  expect_equal(landfall:::as_site(site_lookup("CUVE"))$code, "CUVE")
  expect_equal(landfall:::as_site(list(name = "Somewhere", lon = -60, lat = -65))$code, "SOME")
  expect_null(landfall:::as_site(list(name = "Somewhere", lon = -60, lat = -65))$mapppd_id)
  expect_equal(landfall:::as_site(list(name = "A beach", lon = -62.53, lat = -64.84,
                                       mapppd_id = "NEKO"))$mapppd_id, "NEKO")
  expect_error(landfall:::as_site(42), "A site is")
})

test_that("neighboring sites are found within a radius and sorted by distance", {
  n <- sites_within("NEKO", radius_km = 25)
  expect_true(nrow(n) > 0)
  expect_false("NEKO" %in% n$code)
  expect_true(all(diff(n$dist_km) >= 0))
  expect_true(all(n$dist_km <= 25))
  expect_true("n_counts" %in% names(n))
})

test_that("a point of your own does not drop the MAPPPD site its label resembles", {
  # Neko Harbor is a real neighbor of a point near it, and the point's label
  # must not remove it from the map.
  ridge <- site_point("Neko Ridge", lon = -62.50, lat = -64.80)
  expect_true("NEKO" %in% sites_within(ridge, radius_km = 25)$code)
})
