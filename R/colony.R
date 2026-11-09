# --- Colony counts from MAPPPD -----------------------------------------------------
# MAPPPD records each count with its site, species, season, count type (nests,
# chicks or adults), accuracy class and vantage. A season is the austral
# summer named by the year in which it begins, so a count made in January 2022
# belongs to season 2021.

COUNT_TYPES <- c("nests", "chicks", "adults")

# --- The accuracy classes -----------------------------------------------------
# MAPPPD's accuracy field holds the five-point count-precision scale that
# penguin census counters assign in the field. The mapppdr documentation of
# penguin_obs describes the scores as "assigned on a 5 point scale (usually by
# the observer) that penguin census counters traditionally use to represent
# count precision", citing Croxall and Kirkwood (1979). A class is therefore a
# note the counter made about how precisely they could count that colony. It is
# not an uncertainty estimated from the count and it carries no probability, so
# a range derived from it is not a confidence interval and says nothing about
# where the true population lies.
#
# The published bands, as tabulated by Strycker et al. (2020, Table 2), are:
#
#   class 1  within 5 percent
#   class 2  within 10 percent
#   class 3  within 10 to 15 percent
#   class 4  within 25 to 50 percent
#   class 5  the nearest order of magnitude
#
# Two of those are bands and one is not a percentage at all, so turning a class
# into a range needs a stated policy. This package takes the wider end of each
# band, which gives 15 percent for class 3 and 50 percent for class 4. Class 5
# is multiplicative instead: a count good to the nearest order of magnitude
# sits within a factor of the square root of ten either side, which spans
# exactly one order of magnitude. That last step is this package's reading of
# the category and is not a published interval.
#
# An unrecorded class is NA and yields no range, so an unqualified count is
# never given a precision it was not assigned.
ACCURACY_SHARE  <- c(0.05, 0.10, 0.15, 0.50, NA_real_)
ACCURACY_FACTOR <- c(NA_real_, NA_real_, NA_real_, NA_real_, sqrt(10))
ACCURACY_LABEL  <- c("within 5%", "within 10%", "within 10 to 15%",
                     "within 25 to 50%", "the nearest order of magnitude")

# What a class means, in words, for a caption. NA has no label.
accuracy_label <- function(accuracy) {
  a <- as.integer(accuracy)
  out <- rep(NA_character_, length(a))
  ok <- !is.na(a) & a %in% seq_along(ACCURACY_LABEL)
  out[ok] <- ACCURACY_LABEL[a[ok]]
  out
}

#' The count range implied by a MAPPPD accuracy class
#'
#' A class is the count precision the observer assigned in the field, so the
#' range it implies restates that note and is not a confidence interval.
#' Classes 1 to 4 give a symmetric range from the wider end of the published
#' band. Class 5, the nearest order of magnitude, gives a multiplicative range
#' of the square root of ten either side. An `NA` class gives an `NA` range,
#' because a count with no class recorded has no stated precision.
#'
#' @param count The count.
#' @param accuracy The accuracy class, 1 to 5, or `NA`.
#' @return A data frame with `lo` and `hi`.
#' @examples
#' accuracy_bounds(1000, 2)
#' accuracy_bounds(1000, 1:5)
#' accuracy_bounds(1000, NA)
#' @export
accuracy_bounds <- function(count, accuracy) {
  n <- max(length(count), length(accuracy))
  count <- rep_len(count, n)
  a <- rep_len(as.integer(accuracy), n)
  a[!is.na(a) & !(a %in% seq_along(ACCURACY_SHARE))] <- NA_integer_
  share <- ACCURACY_SHARE[a]
  fac   <- ACCURACY_FACTOR[a]
  lo <- count * (1 - share)
  hi <- count * (1 + share)
  mult <- !is.na(fac)
  lo[mult] <- count[mult] / fac[mult]
  hi[mult] <- count[mult] * fac[mult]
  data.frame(lo = lo, hi = hi)
}

#' The season a date falls in
#'
#' @param date A Date or anything `as.Date()` accepts.
#' @return The season as an integer: the year for dates from July on, and
#'   the year before for dates from January to June.
#' @examples
#' season_of("2026-12-14")
#' season_of("2022-01-29")
#' @export
season_of <- function(date) {
  d <- as.Date(date)
  as.integer(format(d, "%Y")) - as.integer(as.integer(format(d, "%m")) < 7)
}

#' The MAPPPD count record of a site
#'
#' Records are retrieved by the site's validated MAPPPD identifier, which only
#' a site from [site_lookup()], or a [site_point()] given an explicit
#' `mapppd_id`, carries. A point of your own has no MAPPPD identity and returns
#' an empty record, so it can never inherit the history of a site whose code it
#' happens to resemble.
#'
#' @param site A site, as [as_site()] accepts.
#' @param species A species code to keep, or `NULL` for all.
#' @return A data frame with `site_id`, `species_id`, `species`, `season`,
#'   `date`, `type`, `count`, `accuracy`, `vantage` and `citekey`, sorted by
#'   species, season and type. Presence-only rows with no count are left out.
#' @examples
#' head(colony_record("NEKO"))
#' @export
colony_record <- function(site, species = NULL) {
  site <- as_site(site)
  o <- as.data.frame(mapppdr::penguin_obs)
  keep <- c("site_id", "species_id", "species", "season", "date", "type", "count",
            "accuracy", "vantage", "citekey")
  if (is.null(site$mapppd_id)) {
    o$species <- character(nrow(o))
    return(o[0, intersect(keep, names(o))])
  }
  o <- o[o$site_id == site$mapppd_id & !is.na(o$count), ]
  if (!is.null(species)) o <- o[o$species_id %in% toupper(species), ]
  o$species <- species_label(o$species_id)
  o <- o[order(o$species_id, o$season, o$type), ]
  rownames(o) <- NULL
  o[, intersect(keep, names(o))]
}

#' A new colony count
#'
#' Checks and packages a count to add to a site's record.
#'
#' @param count The count, a whole number.
#' @param type `"nests"`, `"chicks"` or `"adults"`.
#' @param species A MAPPPD species code: `ADPE`, `CHPE`, `EMPE`, `GEPE`,
#'   `KIPE` or `MCPE`.
#' @param date The date of the count, or `NULL` when only the season is known.
#' @param season The season, or `NULL` to take it from `date`. Giving a date
#'   and a season that disagree is an error.
#' @param accuracy MAPPPD accuracy class, 1 to 5, or `NA` when the observer
#'   recorded none. There is no default, because an omitted class is unknown
#'   and not the most precise one.
#' @param source A short note on who made the count, for the caption.
#' @return A `landfall_count`: a list with the arguments, the season, and the
#'   count range from [accuracy_bounds()], which is `NA` for an `NA` class.
#' @examples
#' new_count(812, "nests", "GEPE", date = "2026-12-14", accuracy = 2)
#' @export
new_count <- function(count, type = c("nests", "chicks", "adults"), species, date = NULL,
                      season = NULL, accuracy = NA, source = NULL) {
  type <- match.arg(type)
  if (!is.numeric(count) || length(count) != 1 || !is.finite(count) || count < 0 ||
      count != round(count)) {
    stop("count is one finite whole number, zero or more.", call. = FALSE)
  }
  species <- toupper(species)
  if (!species %in% mapppdr::species$species_id || species == "UNPE") {
    stop("species must be one of ", paste(setdiff(mapppdr::species$species_id, "UNPE"),
                                           collapse = ", "), call. = FALSE)
  }
  if (length(accuracy) != 1 || (!is.na(accuracy) && !accuracy %in% 1:5)) {
    stop("accuracy is one MAPPPD class from 1 to 5, or NA when none was recorded.",
         call. = FALSE)
  }
  if (is.null(date) && is.null(season)) stop("Give the date or the season of the count.",
                                            call. = FALSE)
  if (!is.null(date)) {
    date <- as.Date(date)
    if (length(date) != 1 || is.na(date)) stop("date is one date.", call. = FALSE)
  }
  if (is.null(season)) {
    season <- season_of(date)
  } else {
    if (length(season) != 1 || is.na(season) || !is.finite(as.numeric(season))) {
      stop("season is one year.", call. = FALSE)
    }
    if (!is.null(date) && season_of(date) != as.integer(season)) {
      stop(sprintf("The date %s falls in season %d, not season %d. Correct one of them.",
                   format(date), season_of(date), as.integer(season)), call. = FALSE)
    }
  }
  b <- accuracy_bounds(count, accuracy)
  structure(list(count = as.integer(count), type = type, species = species, date = date,
                 season = as.integer(season), accuracy = as.integer(accuracy),
                 lo = b$lo, hi = b$hi, source = source), class = "landfall_count")
}

#' @export
print.landfall_count <- function(x, ...) {
  precision <- if (is.na(x$accuracy)) {
    "no accuracy class recorded"
  } else {
    sprintf("accuracy class %d, %s (%s to %s)", x$accuracy, accuracy_label(x$accuracy),
            fmt_n(x$lo), fmt_n(x$hi))
  }
  cat(sprintf("%s %s of %s, season %d%s, %s\n", fmt_n(x$count), x$type,
              species_label(x$species), x$season,
              if (is.null(x$date)) "" else paste0(" (", fmt_date(x$date), ")"), precision))
  invisible(x)
}

#' The highest recorded count of each season
#'
#' For one species and count type, the highest count recorded in each season
#' that holds one. A season's peak is a single observation, so the accuracy
#' class, date, vantage and citation returned beside it are that same
#' observation's and are never assembled from different counts.
#'
#' The peak is the highest count that was recorded. The package does not select
#' observations by breeding stage, estimate missed nests, or check that two
#' seasons covered the same ground, so the peak is not the count nearest the
#' peak of incubation.
#'
#' @param record The table from [colony_record()].
#' @param species A species code.
#' @param type A count type.
#' @return A data frame with `season`, `peak`, `n_counts` (how many counts that
#'   season held) and the chosen observation's `accuracy`, `date`, `vantage`,
#'   `citekey`, `lo` and `hi`. Where two counts tie, the one with the better
#'   accuracy class wins, and where they tie on that too the earlier row of the
#'   record wins, so the choice is repeatable.
#' @export
season_peaks <- function(record, species, type) {
  r <- record[record$species_id == toupper(species) & record$type == type, , drop = FALSE]
  if (!nrow(r)) {
    return(data.frame(season = integer(0), peak = integer(0), n_counts = integer(0),
                      accuracy = integer(0), date = as.Date(character(0)),
                      vantage = character(0), citekey = character(0),
                      lo = numeric(0), hi = numeric(0), stringsAsFactors = FALSE))
  }
  for (nm in c("date", "vantage", "citekey")) {
    if (!nm %in% names(r)) r[[nm]] <- if (nm == "date") as.Date(NA) else NA_character_
  }
  n_counts <- tapply(r$count, r$season, length)
  # A lower class is a better one, and a count with no class sorts last.
  rank_acc <- ifelse(is.na(r$accuracy), Inf, as.numeric(r$accuracy))
  ord <- order(r$season, -r$count, rank_acc, seq_len(nrow(r)))
  pick <- r[ord, , drop = FALSE]
  pick <- pick[!duplicated(pick$season), , drop = FALSE]
  b <- accuracy_bounds(pick$count, pick$accuracy)
  out <- data.frame(season = as.integer(pick$season), peak = pick$count,
                    n_counts = as.integer(n_counts[as.character(pick$season)]),
                    accuracy = as.integer(pick$accuracy),
                    date = as.Date(pick$date), vantage = as.character(pick$vantage),
                    citekey = as.character(pick$citekey), lo = b$lo, hi = b$hi,
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out[order(out$season), , drop = FALSE]
}

#' How a new count compares with the site's record
#'
#' Compares the new count with the highest recorded count of each earlier
#' season for the same species and count type. Matching the species and the
#' type is all the comparison controls for: a difference between two seasons
#' may also come from when in the breeding cycle each count was made, how much
#' of the colony each one covered, and what method each used.
#'
#' @param record The table from [colony_record()].
#' @param new A [new_count()].
#' @param recent How many of the most recent earlier seasons form the recent
#'   range.
#' @param site The site the record came from, used only to say plainly when a
#'   point has no MAPPPD identity and no record was looked up.
#' @return A list: `n_seasons` (earlier seasons with a count of this type),
#'   `in_mapppd`, `first_season`, `last_season`, `last_peak`, `seasons_ago`,
#'   `change`, `change_pct` (`NA` when the earlier peak was zero), `n_more`,
#'   `n_fewer`, `n_same`, `recent_lo`, `recent_hi`, `recent_n`, `other_types`
#'   (a data frame of the latest season of each other count type) and
#'   `sentences`, the comparison in words.
#' @examples
#' count_context(colony_record("NEKO"),
#'               new_count(812, "nests", "GEPE", season = 2026, accuracy = 2))
#' @export
count_context <- function(record, new, recent = 10, site = NULL) {
  peaks <- season_peaks(record, new$species, new$type)
  earlier <- peaks[peaks$season < new$season, , drop = FALSE]
  ctx <- list(n_seasons = nrow(earlier),
              in_mapppd = is.null(site) || !is.null(site$mapppd_id))
  other <- record[record$species_id == new$species & record$type != new$type, ]
  ctx$other_types <- if (nrow(other)) {
    last_season <- tapply(other$season, other$type, max)
    n_seasons <- tapply(other$season, other$type, function(s) length(unique(s)))
    data.frame(type = names(last_season), last_season = as.integer(last_season),
               n_seasons = as.integer(n_seasons[names(last_season)]),
               stringsAsFactors = FALSE)
  } else NULL
  if (nrow(earlier)) {
    last <- earlier[which.max(earlier$season), ]
    ctx$first_season <- min(earlier$season)
    ctx$last_season  <- last$season
    ctx$last_peak    <- last$peak
    ctx$seasons_ago  <- new$season - last$season
    ctx$change       <- new$count - last$peak
    ctx$change_pct   <- if (last$peak > 0) 100 * ctx$change / last$peak else NA_real_
    ctx$n_more  <- sum(earlier$peak > new$count)
    ctx$n_fewer <- sum(earlier$peak < new$count)
    ctx$n_same  <- sum(earlier$peak == new$count)
    rec <- utils::tail(earlier[order(earlier$season), ], recent)
    ctx$recent_n  <- nrow(rec)
    ctx$recent_lo <- min(rec$peak)
    ctx$recent_hi <- max(rec$peak)
    ctx$recent_first <- min(rec$season)
  }
  ctx$sentences <- context_sentences(ctx, new)
  ctx
}

# The comparison in complete sentences.
context_sentences <- function(ctx, new) {
  word <- c(nests = "nest", chicks = "chick", adults = "adult")[[new$type]]
  s <- character(0)
  if (ctx$n_seasons == 0) {
    s <- c(s, if (!ctx$in_mapppd) {
      sprintf(paste("This point is not a MAPPPD site, so no earlier %s counts of the %s",
                    "were looked up."), word, species_phrase(new$species))
    } else {
      sprintf(paste("The installed MAPPPD dataset holds no earlier season with a recorded",
                    "count of %s for the %s at this site. Counts from this season or from",
                    "later seasons may exist, and this count is not submitted to MAPPPD."),
              new$type, species_phrase(new$species))
    })
  } else {
    s <- c(s, if (ctx$n_seasons == 1) {
      sprintf("MAPPPD holds a %s count of the %s from one earlier season, %d.", word,
              species_phrase(new$species), ctx$last_season)
    } else {
      sprintf("MAPPPD holds %s counts of the %s from %d earlier seasons, %d to %d.", word,
              species_phrase(new$species), ctx$n_seasons, ctx$first_season, ctx$last_season)
    })
    ago <- if (ctx$seasons_ago <= 0) "in the same season" else
      if (ctx$seasons_ago == 1) "one season earlier" else
      sprintf("%d seasons earlier", ctx$seasons_ago)
    highest <- sprintf(paste("the highest recorded %s count of the most recent earlier",
                             "season, %s in %d, %s"),
                       word, fmt_n(ctx$last_peak), ctx$last_season, ago)
    if (ctx$change == 0) {
      s <- c(s, sprintf("The new count equals %s.", highest))
    } else if (is.na(ctx$change_pct)) {
      s <- c(s, sprintf(paste("The new count is %s %s more than %s, and the change as a",
                              "percentage is undefined because that count was zero."),
                        fmt_n(abs(ctx$change)), new$type, highest))
    } else {
      s <- c(s, sprintf("The new count is %s%% %s %s.",
                        formatC(abs(ctx$change_pct), format = "f", digits = 0),
                        if (ctx$change > 0) "above" else "below", highest))
    }
    n <- ctx$n_seasons
    s <- c(s, if (n == 1) {
      sprintf("That season had %s.", if (ctx$n_more) paste("more", new$type) else
        if (ctx$n_fewer) paste("fewer", new$type) else "the same number")
    } else if (ctx$n_more == n) {
      sprintf("All %d earlier seasons had more %s.", n, new$type)
    } else if (ctx$n_fewer == n) {
      sprintf("All %d earlier seasons had fewer %s.", n, new$type)
    } else {
      sprintf("Of the %d earlier seasons, %d had more %s and %d had fewer%s.", n, ctx$n_more,
              new$type, ctx$n_fewer,
              if (ctx$n_same) sprintf(", and %d had the same number", ctx$n_same) else "")
    })
    if (ctx$recent_n >= 3) {
      where <- if (new$count < ctx$recent_lo) "below" else
        if (new$count > ctx$recent_hi) "above" else "within"
      s <- c(s, sprintf(paste("The %d most recent earlier seasons with a count, %d to %d,",
                              "ranged from %s to %s %s, and the new count is %s that range."),
                        ctx$recent_n, ctx$recent_first, ctx$last_season, fmt_n(ctx$recent_lo),
                        fmt_n(ctx$recent_hi), new$type, where))
    }
  }
  if (!is.null(ctx$other_types)) {
    o <- ctx$other_types
    s <- c(s, sprintf("The site also has %s.",
                      paste(sprintf("%s counts from %d season%s, the last in %d",
                                    sub("s$", "", o$type), o$n_seasons,
                                    ifelse(o$n_seasons == 1, "", "s"), o$last_season),
                            collapse = ", and ")))
  }
  s
}
