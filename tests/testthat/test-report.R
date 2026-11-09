test_that("a report's default name identifies the observation", {
  s <- site_lookup("NEKO")
  n1 <- new_count(812, "nests", "GEPE", date = "2026-12-14", accuracy = 2)
  n2 <- new_count(900, "chicks", "GEPE", date = "2026-12-14", accuracy = 2)
  n3 <- new_count(40, "nests", "KIPE", date = "2026-12-14", accuracy = 2)
  n4 <- new_count(820, "nests", "GEPE", date = "2026-12-20", accuracy = 2)
  stems <- vapply(list(n1, n2, n3, n4), function(n) landfall:::report_stem(s, n), character(1))
  # A different count type, a different species and a different day are each
  # enough to make a different file, so no observation writes over another.
  expect_equal(length(unique(stems)), 4)
  expect_equal(stems[1], "neko_gepe_nests_20261214_report")
  # A count known only by its season still names the season.
  expect_match(landfall:::report_stem(s, new_count(812, "nests", "GEPE", season = 2026)),
               "season2026")
})

test_that("save_figure will not quietly replace an existing report", {
  skip_if_not(capabilities("cairo"), "cairo is needed for the PDF")
  dir <- withr::local_tempdir()
  p <- ggplot2::ggplot() + ggplot2::geom_blank()
  first <- suppressMessages(save_figure(p, "demo", dir, width = 3, height = 2, dpi = 72))
  expect_equal(names(first), c("png", "pdf"))
  expect_true(all(file.exists(first)))
  expect_error(suppressMessages(save_figure(p, "demo", dir, width = 3, height = 2, dpi = 72)),
               "already exists")
  second <- suppressMessages(save_figure(p, "demo", dir, width = 3, height = 2, dpi = 72,
                                         overwrite = TRUE))
  expect_true(all(file.exists(second)))
  expect_equal(unname(first), unname(second))
})

test_that("save_figure writes nothing when no output folder is set", {
  p <- ggplot2::ggplot() + ggplot2::geom_blank()
  expect_null(save_figure(p, "demo", out_dir = NULL))
})

test_that("the mapppdr version is reported whole", {
  v <- landfall:::mapppd_version()
  expect_match(v, "^v")
  # The installed version is reported in full rather than cut to two parts, so
  # a caption cannot claim a release the counts did not come from.
  expect_match(v, as.character(utils::packageVersion("mapppdr")), fixed = TRUE)
})
