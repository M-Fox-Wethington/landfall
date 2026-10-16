# --- Colony counts from MAPPPD -----------------------------------------------------
# MAPPPD records each count with its site, species, season, count type (nests,
# chicks or adults), accuracy class and vantage. A season is the austral
# summer named by the year in which it begins, so a count made in January 2022
# belongs to season 2021.

COUNT_TYPES <- c("nests", "chicks", "adults")

# MAPPPD accuracy classes 1 to 5 as the half-width of the count's range, as a
# share of the count: within 5%, 10%, 15%, 25% and 50%.
ACCURACY_SHARE <- c(0.05, 0.10, 0.15, 0.25, 0.50)

#' The count range implied by a MAPPPD accuracy class
#'
#' MAPPPD grades each count 1 to 5, meaning the true value is within 5%, 10%,
#' 15%, 25% or 50% of the count.
#'
#' @param count The count.
#' @param accuracy The accuracy class, 1 to 5. `NA` gives an `NA` range.
#' @return A data frame with `lo` and `hi`.
#' @examples
#' accuracy_bounds(1000, 2)
#' @export
accuracy_bounds <- function(count, accuracy) {
  share <- ACCURACY_SHARE[as.integer(accuracy)]
  data.frame(lo = count * (1 - share), hi = count * (1 + share))
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
  o <- o[o$site_id == site$code & !is.na(o$count), ]
  if (!is.null(species)) o <- o[o$species_id %in% species, ]
  o$species <- species_label(o$species_id)
  o <- o[order(o$species_id, o$season, o$type), ]
  rownames(o) <- NULL
  o[, intersect(c("site_id", "species_id", "species", "season", "date", "type", "count",
                  "accuracy", "vantage", "citekey"), names(o))]
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
#' @param season The season, or `NULL` to take it from `date`.
#' @param accuracy MAPPPD accuracy class, 1 to 5.
#' @param source A short note on who made the count, for the caption.
#' @return A `landfall_count`: a list with the arguments, the season, and the
#'   count range from [accuracy_bounds()].
#' @examples
#' new_count(812, "nests", "GEPE", date = "2026-12-14")
#' @export
new_count <- function(count, type = c("nests", "chicks", "adults"), species, date = NULL,
                      season = NULL, accuracy = 1, source = NULL) {
  type <- match.arg(type)
  stopifnot(is.numeric(count), length(count) == 1, count >= 0, count == round(count))
  species <- toupper(species)
  if (!species %in% mapppdr::species$species_id || species == "UNPE") {
    stop("species must be one of ", paste(setdiff(mapppdr::species$species_id, "UNPE"),
                                           collapse = ", "), call. = FALSE)
  }
  if (!accuracy %in% 1:5) stop("accuracy is a MAPPPD class from 1 to 5.", call. = FALSE)
  if (is.null(date) && is.null(season)) stop("Give the date or the season of the count.",
                                            call. = FALSE)
  if (!is.null(date)) date <- as.Date(date)
  if (is.null(season)) season <- season_of(date)
  b <- accuracy_bounds(count, accuracy)
  structure(list(count = as.integer(count), type = type, species = species, date = date,
                 season = as.integer(season), accuracy = as.integer(accuracy),
                 lo = b$lo, hi = b$hi, source = source), class = "landfall_count")
}

#' @export
print.landfall_count <- function(x, ...) {
  cat(sprintf("%s %s of %s, season %d%s, accuracy class %d (%s to %s)\n", fmt_n(x$count),
              x$type, species_label(x$species), x$season,
              if (is.null(x$date)) "" else paste0(" (", fmt_date(x$date), ")"), x$accuracy,
              fmt_n(x$lo), fmt_n(x$hi)))
  invisible(x)
}

#' The peak count of each season
#'
#' For one species and count type, the highest count in each season, which
#' for nests is the count nearest the peak of incubation and the best estimate
#' of breeding pairs.
#'
#' @param record The table from [colony_record()].
#' @param species A species code.
#' @param type A count type.
#' @return A data frame with `season`, `peak`, `n_counts` (counts that
#'   season), `accuracy` (the best class among them), `lo` and `hi`.
#' @export
season_peaks <- function(record, species, type) {
  r <- record[record$species_id == species & record$type == type, ]
  if (!nrow(r)) {
    return(data.frame(season = integer(0), peak = integer(0), n_counts = integer(0),
                      accuracy = integer(0), lo = numeric(0), hi = numeric(0)))
  }
  out <- r |>
    group_by(season) |>
    summarise(peak = max(count), n_counts = dplyr::n(),
              accuracy = suppressWarnings(min(accuracy, na.rm = TRUE)), .groups = "drop") |>
    as.data.frame()
  out$accuracy[!is.finite(out$accuracy)] <- NA
  b <- accuracy_bounds(out$peak, out$accuracy)
  out$lo <- b$lo
  out$hi <- b$hi
  out[order(out$season), ]
}

#' How a new count compares with the site's record
#'
#' Compares the new count with the earlier seasons' peak counts of the same
#' species and type, so the comparison is like for like.
#'
#' @param record The table from [colony_record()].
#' @param new A [new_count()].
#' @param recent How many of the latest earlier seasons form the recent range.
#' @return A list: `n_seasons` (earlier seasons with a count of this type),
#'   `first_season`, `last_season`, `last_peak`, `seasons_ago`, `change`,
#'   `change_pct`, `n_more`, `n_fewer`, `n_same`, `recent_lo`, `recent_hi`,
#'   `recent_n`, `other_types` (a data frame of the latest season of each
#'   other count type) and `sentences`, the comparison in words.
#' @examples
#' count_context(colony_record("NEKO"), new_count(812, "nests", "GEPE", season = 2026))
#' @export
count_context <- function(record, new, recent = 10) {
  peaks <- season_peaks(record, new$species, new$type)
  earlier <- peaks[peaks$season < new$season, ]
  ctx <- list(n_seasons = nrow(earlier))
  other <- record[record$species_id == new$species & record$type != new$type, ]
  ctx$other_types <- if (nrow(other)) {
    other |> group_by(type) |> summarise(last_season = max(season), n_seasons = dplyr::n_distinct(season),
                                        .groups = "drop") |> as.data.frame()
  } else NULL
  if (nrow(earlier)) {
    last <- earlier[which.max(earlier$season), ]
    ctx$first_season <- min(earlier$season)
    ctx$last_season  <- last$season
    ctx$last_peak    <- last$peak
    ctx$seasons_ago  <- new$season - last$season
    ctx$change       <- new$count - last$peak
    ctx$change_pct   <- 100 * ctx$change / last$peak
    ctx$n_more  <- sum(earlier$peak > new$count)
    ctx$n_fewer <- sum(earlier$peak < new$count)
    ctx$n_same  <- sum(earlier$peak == new$count)
    rec <- tail(earlier[order(earlier$season), ], recent)
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
    s <- c(s, sprintf("This is the first %s count of the %s at this site in MAPPPD.", word, species_phrase(new$species)))
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
    if (ctx$change == 0) {
      s <- c(s, sprintf("The new count equals the last %s count, %s in %d, %s.", word,
                        fmt_n(ctx$last_peak), ctx$last_season, ago))
    } else {
      s <- c(s, sprintf("The new count is %s%% %s the last %s count, %s in %d, %s.",
                        formatC(abs(ctx$change_pct), format = "f", digits = 0),
                        if (ctx$change > 0) "above" else "below", word,
                        fmt_n(ctx$last_peak), ctx$last_season, ago))
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
      where <- if (new$count < ctx$recent_lo) "below" else if (new$count > ctx$recent_hi) "above" else "within"
      s <- c(s, sprintf("The last %d seasons with a count, %d to %d, ranged from %s to %s %s, and the new count is %s that range.",
                        ctx$recent_n, ctx$recent_first, ctx$last_season, fmt_n(ctx$recent_lo),
                        fmt_n(ctx$recent_hi), new$type, where))
    }
  }
  if (!is.null(ctx$other_types)) {
    o <- ctx$other_types
    s <- c(s, sprintf("The site also has %s.", paste(sprintf("%s counts from %d season%s, the last in %d",
                      sub("s$", "", o$type), o$n_seasons, ifelse(o$n_seasons == 1, "", "s"),
                      o$last_season), collapse = ", and ")))
  }
  s
}
