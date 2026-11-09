# --- The site report ---------------------------------------------------------------

# A default file name that identifies the observation and not just the site and
# the season, so two counts of different species, different count types or
# different days at one site cannot write over each other.
report_stem <- function(site, new) {
  when <- if (is.null(new$date)) paste0("season", new$season) else format(new$date, "%Y%m%d")
  paste(tolower(site$code), tolower(new$species), new$type, when, "report", sep = "_")
}

# The full mapppdr version, with the data commit when the install records one,
# because the counts come from whichever release is installed.
mapppd_version <- function() {
  v <- tryCatch(as.character(utils::packageVersion("mapppdr")), error = function(e) NA)
  if (is.na(v)) return("(version unknown)")
  commit <- tryCatch(utils::packageDescription("mapppdr")[["Config/mapppdr/data_commit"]],
                     error = function(e) NULL)
  if (is.null(commit) || all(is.na(commit))) paste0("v", v) else paste0("v", v, ", data ", commit)
}

#' Figure: a site, its surroundings and a new colony count
#'
#' One call for a site code and a count. The figure holds the relief map of
#' the site's surroundings on the left, the species' season-by-season record
#' from MAPPPD with the new count on it at the top right, and the comparison
#' with earlier seasons in words below it. When the site has counts of other
#' species, each gets a smaller panel.
#'
#' @param site A site, as [as_site()] accepts: a four-letter MAPPPD code, or
#'   the result of [site_lookup()] or [site_point()]. A point of your own has
#'   no MAPPPD identity, so no count record is read for it and the comparison
#'   says so.
#' @param count The new count, a whole number.
#' @param type `"nests"`, `"chicks"` or `"adults"`.
#' @param species A MAPPPD species code, or `NULL` when the site has counts
#'   of only one species. Case does not matter.
#' @param date The date of the count, or `NULL` when only the season is known.
#' @param season The season, or `NULL` to take it from `date`.
#' @param accuracy MAPPPD accuracy class of the count, 1 to 5, or `NA` when
#'   none was recorded. There is no default, because an omitted class is
#'   unknown and not the most precise one.
#' @param source A short note on who made the count, for the caption.
#' @param radius_km Half-width of the map in kilometers.
#' @param lon_0 Central meridian of the map projection, or `NULL` for the
#'   site's own.
#' @param places A data frame of place names for the map, or `NULL`.
#' @param label_sites Mark the other MAPPPD sites within the map.
#' @param title Figure title, or `NULL` for the site's name.
#' @param out_dir Folder to write into, or `NULL` to write nothing.
#' @param name File name stem. The default identifies the site, the species,
#'   the count type and the date, so one observation cannot overwrite another.
#' @param overwrite Write over files of the same name that already exist.
#' @param width,height Size in inches.
#' @param dpi Resolution of the PNG.
#' @param paths The list from [landfall_paths()].
#' @return The cowplot object, invisibly, with attributes `context` (the list
#'   from [count_context()]), `record`, `count`, `layers` and `files`, the
#'   paths written.
#' @examples
#' \dontrun{
#' site_report("NEKO", count = 812, type = "nests", species = "GEPE",
#'             date = "2026-12-14", accuracy = 2, source = "HX expedition, ground count")
#' }
#' @export
site_report <- function(site, count, type = c("nests", "chicks", "adults"), species = NULL,
                        date = NULL, season = NULL, accuracy = NA, source = NULL,
                        radius_km = 25, lon_0 = NULL, places = NULL, label_sites = TRUE,
                        title = NULL, out_dir = landfall_paths()$output, name = NULL,
                        overwrite = FALSE, width = 16, height = 9, dpi = 300,
                        paths = landfall_paths()) {
  site <- as_site(site)
  type <- match.arg(type)
  record <- colony_record(site)
  if (is.null(species)) {
    present <- setdiff(unique(record$species_id), "UNPE")
    if (length(present) == 1) {
      species <- present
    } else if (!length(present) && is.null(site$mapppd_id)) {
      stop("This point is not a MAPPPD site, so it has no record to take the species ",
           "from; say which species the count is for.", call. = FALSE)
    } else {
      stop("The site has counts of ",
           if (length(present)) paste(present, collapse = ", ") else "no species",
           "; say which species the count is for.", call. = FALSE)
    }
  }
  new <- new_count(count, type, species, date = date, season = season, accuracy = accuracy,
                   source = source)
  # From here on the normalized code from the count is the one that is used, so
  # a lowercase species cannot miss the record it is meant to match.
  species <- new$species
  ctx <- count_context(record, new, site = site)

  w <- site_window(site, radius_km, lon_0 = lon_0)
  L <- site_map_layers(w, paths)
  # The map is drawn into a slot of the figure rather than filling it, so its
  # panel is that slot less the map's own margins and edge labels. The size is
  # measured here, because the reservation that keeps the names off the scale
  # bar and the north arrow is in inches of drawn panel.
  map_h <- 0.78
  map_w <- map_h * height / width
  map <- site_map_plot(L, places = places, label_sites = label_sites, text_pt = 8.5,
                       panel_in = map_panel_inches(w, map_w * width, map_h * height))

  # Other species get a panel when the site holds a nest or chick count of
  # them, or counts from more than one season; a single count of adults, such
  # as a vagrant, does not make a colony.
  others <- setdiff(unique(record$species_id), c(species, "UNPE"))
  others <- others[vapply(others, function(sp) {
    r <- record[record$species_id == sp, ]
    any(r$type %in% c("nests", "chicks")) || length(unique(r$season)) > 1
  }, logical(1))]
  main_panel <- panel_colony_trend(record, species, type = type, new = new, base_size = 11)
  other_panels <- lapply(others, function(sp) {
    t <- names(which.max(table(record$type[record$species_id == sp])))
    panel_colony_trend(record, sp, type = t, base_size = 9)
  })

  if (is.null(title)) title <- site$name
  subtitle <- sprintf("%s%s. New count: %s %s of the %s, %s%s.",
                      if (!is.null(site$mapppd_id)) paste0("MAPPPD site ", site$mapppd_id, ", ")
                      else "",
                      paste0(if (is.null(site$region)) "" else paste0(site$region, ", "),
                             fmt_latlon(site$lon, site$lat)),
                      fmt_n(new$count), new$type, species_phrase(species),
                      if (is.null(new$date)) paste("season", new$season) else fmt_date(new$date),
                      if (is.null(source)) "" else paste0(", ", source))
  context_text <- paste(strwrap(paste(ctx$sentences, collapse = " "),
                                width = if (length(others)) 92 else 80), collapse = "\n")
  precision <- if (is.na(new$accuracy)) {
    "no accuracy class was recorded for the new count, so it is drawn without a range"
  } else {
    sprintf(paste("the bar on the new count is its MAPPPD accuracy class %d, %s, which is",
                  "the precision the observer assigned and not a confidence interval"),
            new$accuracy, accuracy_label(new$accuracy))
  }
  # draw_label() does not wrap, so the caption is wrapped here. The width is in
  # characters and 250 of them span about 13 of the figure's 16 inches at 8 pt,
  # which leaves a margin at every length the sources run to.
  caption <- paste(strwrap(paste0(
    "Colony counts: Antarctic Penguin Biogeography Project (Che-Castaldo, Humphries and ",
    "Lynch 2023) through mapppdr ", mapppd_version(), ", CC BY 4.0; the MAPPPD application ",
    "is described by Humphries et al. (2017). The line joins the highest recorded count of ",
    "each season, and ", precision, ". Map: ", radius_km, " km around the site. ",
    map_caption(L)), width = 250), collapse = "\n")

  right_x <- 0.05 + map_w + 0.03
  right_w <- 0.97 - right_x
  n_other <- length(other_panels)
  fig <- cowplot::ggdraw() +
    theme(plot.background = element_rect(fill = "white", color = NA)) +
    cowplot::draw_label(title, x = 0.05, y = 0.965, hjust = 0, vjust = 1, size = 22,
                        fontface = "bold", color = COLORS$title) +
    cowplot::draw_label(subtitle, x = 0.05, y = 0.915, hjust = 0, vjust = 1, size = 11,
                        color = COLORS$subtitle) +
    cowplot::draw_plot(map, x = 0.05, y = 0.09, width = map_w, height = map_h) +
    cowplot::draw_label(caption, x = 0.05, y = 0.025, hjust = 0, vjust = 0, size = 8,
                        color = COLORS$muted, lineheight = 1.25)
  if (n_other == 0) {
    fig <- fig +
      cowplot::draw_plot(main_panel, x = right_x, y = 0.36, width = right_w, height = 0.51) +
      cowplot::draw_label(context_text, x = right_x + 0.01, y = 0.32, hjust = 0, vjust = 1,
                          size = 12.5, color = COLORS$title, lineheight = 1.35)
  } else {
    each <- min(0.26 / n_other, 0.20)
    fig <- fig +
      cowplot::draw_plot(main_panel, x = right_x, y = 0.49, width = right_w, height = 0.38)
    for (i in seq_len(n_other)) {
      fig <- fig + cowplot::draw_plot(other_panels[[i]], x = right_x,
                                      y = 0.49 - i * each, width = right_w, height = each)
    }
    fig <- fig +
      cowplot::draw_label(context_text, x = right_x + 0.01, y = 0.49 - n_other * each - 0.03,
                          hjust = 0, vjust = 1, size = 11, color = COLORS$title, lineheight = 1.3)
  }

  if (is.null(name)) name <- report_stem(site, new)
  files <- save_figure(fig, name, out_dir, width = width, height = height, dpi = dpi,
                       bg = "white", overwrite = overwrite)
  message(paste(ctx$sentences, collapse = "\n"))
  invisible(structure(fig, context = ctx, record = record, count = new, layers = L,
                      files = files))
}
