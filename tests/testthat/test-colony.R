synthetic_record <- function() {
  data.frame(
    site_id = "TEST", species_id = c(rep("GEPE", 9), "CHPE", "CHPE"),
    species = c(rep("Gentoo penguin", 9), "Chinstrap penguin", "Chinstrap penguin"),
    season = c(2010, 2012, 2012, 2015, 2015, 2018, 2018, 2021, 2021, 2015, 2019),
    date = as.Date(NA),
    type = c("nests", "nests", "nests", "nests", "chicks", "nests", "chicks", "nests",
             "nests", "nests", "nests"),
    count = c(900, 1000, 1100, 1200, 1700, 950, 1500, 800, 850, 300, 320),
    accuracy = c(1, 1, 2, 1, 1, 1, 1, 2, 1, 3, 3),
    vantage = "ground", citekey = "test", stringsAsFactors = FALSE)
}

test_that("accuracy classes give the published MAPPPD bands", {
  b <- accuracy_bounds(1000, 1:4)
  expect_equal(b$lo, c(950, 900, 850, 500))
  expect_equal(b$hi, c(1050, 1100, 1150, 1500))
})

test_that("class 5 is an order of magnitude and not a percentage", {
  b <- accuracy_bounds(1000, 5)
  expect_equal(b$lo, 1000 / sqrt(10))
  expect_equal(b$hi, 1000 * sqrt(10))
  # One order of magnitude from end to end.
  expect_equal(b$hi / b$lo, 10)
})

test_that("an unrecorded class invents no precision", {
  expect_true(is.na(accuracy_bounds(1000, NA)$lo))
  expect_true(is.na(accuracy_bounds(1000, NA)$hi))
  expect_true(is.na(accuracy_bounds(1000, 9)$lo))
  n <- new_count(812, "nests", "GEPE", season = 2026)
  expect_true(is.na(n$accuracy))
  expect_true(is.na(n$lo))
  expect_output(print(n), "no accuracy class recorded")
})

test_that("a season is the year the austral summer begins", {
  expect_equal(season_of("2026-12-14"), 2026L)
  expect_equal(season_of("2027-01-20"), 2026L)
  expect_equal(season_of("2022-01-29"), 2021L)
  expect_equal(season_of("2026-07-01"), 2026L)
})

test_that("season peaks take the highest count of each season for one type", {
  p <- season_peaks(synthetic_record(), "GEPE", "nests")
  expect_equal(p$season, c(2010L, 2012L, 2015L, 2018L, 2021L))
  expect_equal(p$peak, c(900, 1100, 1200, 950, 850))
  expect_equal(p$n_counts, c(1L, 2L, 1L, 1L, 2L))
  expect_equal(nrow(season_peaks(synthetic_record(), "GEPE", "adults")), 0)
})

test_that("a season's peak keeps its own observation's precision", {
  # 2012 holds 1,000 at class 1 and 1,100 at class 2. The peak is 1,100, so the
  # class is 2 and the range is 1,100 plus or minus 10 percent, not the class 1
  # of a count that was not selected.
  p <- season_peaks(synthetic_record(), "GEPE", "nests")
  twelve <- p[p$season == 2012, ]
  expect_equal(twelve$peak, 1100)
  expect_equal(twelve$accuracy, 2L)
  expect_equal(c(twelve$lo, twelve$hi), c(990, 1210))
  # 2021 holds 800 at class 2 and 850 at class 1, so there the better class is
  # also the selected one.
  twentyone <- p[p$season == 2021, ]
  expect_equal(c(twentyone$peak, twentyone$accuracy), c(850, 1))
})

test_that("a season's peak carries its date, vantage and citation", {
  r <- synthetic_record()
  r$vantage[r$season == 2012 & r$count == 1100] <- "uav"
  r$citekey[r$season == 2012 & r$count == 1100] <- "picked"
  p <- season_peaks(r, "GEPE", "nests")
  expect_equal(p$vantage[p$season == 2012], "uav")
  expect_equal(p$citekey[p$season == 2012], "picked")
  expect_true(all(c("date", "vantage", "citekey") %in% names(p)))
})

test_that("a tied maximum resolves by accuracy class and then by order", {
  r <- synthetic_record()
  r$count[r$season == 2012] <- c(1100, 1100)
  r$accuracy[r$season == 2012] <- c(3, 2)
  r$citekey[r$season == 2012] <- c("worse", "better")
  p <- season_peaks(r, "GEPE", "nests")
  expect_equal(p$accuracy[p$season == 2012], 2L)
  expect_equal(p$citekey[p$season == 2012], "better")
  # Tied on the count and on the class, the earlier row wins.
  r$accuracy[r$season == 2012] <- c(2, 2)
  p2 <- season_peaks(r, "GEPE", "nests")
  expect_equal(p2$citekey[p2$season == 2012], "worse")
})

test_that("a count with no class is ranked last among ties", {
  r <- synthetic_record()
  r$count[r$season == 2012] <- c(1100, 1100)
  r$accuracy[r$season == 2012] <- c(NA, 4)
  r$citekey[r$season == 2012] <- c("unclassed", "classed")
  p <- season_peaks(r, "GEPE", "nests")
  expect_equal(p$citekey[p$season == 2012], "classed")
})

test_that("new_count checks its inputs and derives the season", {
  n <- new_count(812, "nests", "gepe", date = "2026-12-14", accuracy = 2)
  expect_s3_class(n, "landfall_count")
  expect_equal(n$season, 2026L)
  expect_equal(n$species, "GEPE")
  expect_equal(c(n$lo, n$hi), c(812 * 0.9, 812 * 1.1))
  expect_error(new_count(812, "nests", "GEPE"), "date or the season")
  expect_error(new_count(812, "nests", "UNPE", season = 2026), "species must be")
  expect_error(new_count(812.5, "nests", "GEPE", season = 2026), "whole number")
  expect_error(new_count(812, "nests", "GEPE", season = 2026, accuracy = 6), "1 to 5")
  expect_output(print(n), "812 nests of Gentoo penguin, season 2026")
})

test_that("a count has to be one finite whole number", {
  expect_error(new_count(Inf, "nests", "GEPE", season = 2026), "finite")
  expect_error(new_count(NA_real_, "nests", "GEPE", season = 2026), "finite")
  expect_error(new_count(c(1, 2), "nests", "GEPE", season = 2026), "whole number")
  expect_error(new_count(-5, "nests", "GEPE", season = 2026), "zero or more")
})

test_that("a date and a season that disagree are refused", {
  expect_error(new_count(812, "nests", "GEPE", date = "2026-12-14", season = 2025),
               "falls in season 2026")
  # The same date and season together are fine.
  n <- new_count(812, "nests", "GEPE", date = "2026-12-14", season = 2026)
  expect_equal(n$season, 2026L)
})

test_that("count_context compares like with like", {
  r <- synthetic_record()
  ctx <- count_context(r, new_count(1000, "nests", "GEPE", season = 2026))
  expect_equal(ctx$n_seasons, 5)
  expect_equal(ctx$first_season, 2010)
  expect_equal(ctx$last_season, 2021)
  expect_equal(ctx$last_peak, 850)
  expect_equal(ctx$seasons_ago, 5)
  expect_equal(ctx$change, 150)
  expect_equal(ctx$change_pct, 100 * 150 / 850)
  expect_equal(ctx$n_more, 2)     # 1100 and 1200
  expect_equal(ctx$n_fewer, 3)    # 900, 950, 850
  expect_equal(ctx$n_same, 0)
  expect_equal(c(ctx$recent_lo, ctx$recent_hi), c(850, 1200))
  expect_equal(ctx$other_types$type, "chicks")
  expect_equal(ctx$other_types$last_season, 2018)
  s <- ctx$sentences
  expect_match(s[1], "5 earlier seasons, 2010 to 2021")
  expect_match(s[2], "18% above the highest recorded nest count of the most recent earlier",
               fixed = TRUE)
  expect_match(s[2], "850 in 2021, 5 seasons earlier", fixed = TRUE)
  expect_match(s[3], "2 had more nests and 3 had fewer")
  expect_match(s[4], "within that range")
  expect_match(s[5], "chick counts from 2 seasons, the last in 2018")
})

test_that("a change from a zero count is reported without a percentage", {
  r <- synthetic_record()
  r$count[r$season == 2021] <- c(0, 0)
  ctx <- count_context(r, new_count(50, "nests", "GEPE", season = 2026))
  expect_equal(ctx$last_peak, 0)
  expect_true(is.na(ctx$change_pct))
  expect_equal(ctx$change, 50)
  expect_match(ctx$sentences[2], "percentage is undefined because that count was zero",
               fixed = TRUE)
  expect_false(any(grepl("Inf", ctx$sentences)))
})

test_that("count_context ignores seasons at or after the new count", {
  r <- synthetic_record()
  ctx <- count_context(r, new_count(1000, "nests", "GEPE", season = 2015))
  expect_equal(ctx$n_seasons, 2)
  expect_equal(ctx$last_season, 2012)
})

test_that("no earlier season is said without claiming it is a first record", {
  r <- synthetic_record()
  ctx <- count_context(r, new_count(40, "adults", "GEPE", season = 2026))
  expect_equal(ctx$n_seasons, 0)
  expect_match(ctx$sentences[1], "no earlier season with a recorded count of adults",
               fixed = TRUE)
  expect_match(ctx$sentences[1], "not submitted to MAPPPD", fixed = TRUE)
  expect_match(ctx$sentences[2], "nest counts from 5 seasons")
})

test_that("a point with no MAPPPD identity says so instead of claiming a first count", {
  p <- site_point("Neko Ridge", lon = -62.50, lat = -64.80)
  r <- colony_record(p)
  expect_equal(nrow(r), 0)
  ctx <- count_context(r, new_count(40, "nests", "GEPE", season = 2026), site = p)
  expect_false(ctx$in_mapppd)
  expect_match(ctx$sentences[1], "not a MAPPPD site", fixed = TRUE)
})

test_that("a tie is counted as the same", {
  r <- synthetic_record()
  ctx <- count_context(r, new_count(1200, "nests", "GEPE", season = 2026))
  expect_equal(ctx$n_same, 1)
  expect_match(ctx$sentences[3], "1 had the same number")
})

test_that("a count above or below every earlier season is said plainly", {
  r <- synthetic_record()
  expect_match(count_context(r, new_count(2000, "nests", "GEPE", season = 2026))$sentences[3],
               "All 5 earlier seasons had fewer nests")
  expect_match(count_context(r, new_count(100, "nests", "GEPE", season = 2026))$sentences[3],
               "All 5 earlier seasons had more nests")
  one <- count_context(r, new_count(500, "nests", "CHPE", season = 2026))
  expect_equal(one$n_seasons, 2)
  ctx1 <- count_context(r[r$season != 2019, ], new_count(500, "nests", "CHPE", season = 2026))
  expect_match(ctx1$sentences[1], "from one earlier season, 2015")
  expect_match(ctx1$sentences[3], "That season had fewer nests")
})

test_that("the real MAPPPD record reads for a known site", {
  r <- colony_record("NEKO")
  expect_true(all(c("species_id", "season", "type", "count") %in% names(r)))
  expect_true(all(r$site_id == "NEKO"))
  expect_true("GEPE" %in% r$species_id)
  p <- season_peaks(r, "GEPE", "nests")
  expect_true(all(diff(p$season) > 0))
  expect_equal(p$peak[p$season == 2008], 1431)
})

test_that("a lowercase species reads the same record as an uppercase one", {
  expect_equal(nrow(colony_record("NEKO", species = "gepe")),
               nrow(colony_record("NEKO", species = "GEPE")))
  r <- colony_record("NEKO")
  expect_equal(nrow(season_peaks(r, "gepe", "nests")), nrow(season_peaks(r, "GEPE", "nests")))
})
