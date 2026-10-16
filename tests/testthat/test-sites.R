test_that("a MAPPPD code looks up its site", {
  s <- site_lookup("neko")
  expect_s3_class(s, "landfall_site")
  expect_equal(s$code, "NEKO")
  expect_match(s$name, "Neko Harbor")
  expect_equal(s$lat, -64.8383, tolerance = 1e-4)
  expect_equal(s$lon, -62.5327, tolerance = 1e-4)
  expect_output(print(s), "NEKO")
  expect_error(site_lookup("NEKX"), "No MAPPPD site")
})

test_that("a named point makes a site", {
  s <- site_point("Stonington Island", lon = -67.0, lat = -68.18)
  expect_equal(s$code, "STON")
  expect_equal(s$source, "user")
  expect_error(site_point("Too far north", lon = 0, lat = -40))
})

test_that("as_site accepts codes, sites and lists", {
  expect_equal(landfall:::as_site("NEKO")$code, "NEKO")
  expect_equal(landfall:::as_site(site_lookup("CUVE"))$code, "CUVE")
  expect_equal(landfall:::as_site(list(name = "Somewhere", lon = -60, lat = -65))$code, "SOME")
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
