# --- The site report ---------------------------------------------------------------

#' Figure: a site, its surroundings and a new colony count
#'
#' One call for a site code and a count. The figure holds the relief map of
#' the site's surroundings on the left, the species' season-by-season record
#' from MAPPPD with the new count on it at the top right, and the comparison
#' with earlier seasons in words below it. When the site has counts of other
#' species, each gets a smaller panel.
#'
#' @param site A site, as [as_site()] accepts: a four-letter MAPPPD code, or
#'   the result of [site_lookup()] or [site_point()].
#' @param count The new count, a whole number.
#' @param type `"nests"`, `"chicks"` or `"adults"`.
#' @param species A MAPPPD species code, or `NULL` when the site has counts
#'   of only one species.
#' @param date The date of the count, or `NULL` when only the season is known.
#' @param season The season, or `NULL` to take it from `date`.
#' @param accuracy MAPPPD accuracy class of the count, 1 to 5.
#' @param source A short note on who made the count, for the caption.
#' @param radius_km Half-width of the map in kilometers.
#' @param lon_0 Central meridian of the map projection, or `NULL` for the
#'   site's own.
#' @param places A data frame of place names for the map, or `NULL`.
#' @param label_sites Mark the other MAPPPD sites within the map.
#' @param title Figure title, or `NULL` for the site's name.
#' @param out_dir Folder to write into, or `NULL` to write nothing.
#' @param name File name stem.
#' @param width,height Size in inches.
#' @param dpi Resolution of the PNG.
#' @param paths The list from [landfall_paths()].
#' @return The cowplot object, invisibly, with attributes `context` (the list
#'   from [count_context()]), `record`, `count` and `layers`.
#' @examples
#' \dontrun{
#' site_report("NEKO", count = 812, type = "nests", species = "GEPE",
#'             date = "2026-12-14", accuracy = 2, source = "HX expedition, ground count")
#' }
#' @export
site_report <- function(site, count, type = c("nests", "chicks", "adults"), species = NULL,
                        date = NULL, season = NULL, accuracy = 1, source = NULL,
                        radius_km = 25, lon_0 = NULL, places = NULL, label_sites = TRUE,
                        title = NULL, out_dir = landfall_paths()$output, name = NULL,
                        width = 16, height = 9, dpi = 300, paths = landfall_paths()) {
  site <- as_site(site)
  type <- match.arg(type)
  record <- colony_record(site)
  if (is.null(species)) {
    present <- setdiff(unique(record$species_id), "UNPE")
    if (length(present) == 1) species <- present else
      stop("The site has counts of ", if (length(present)) paste(present, collapse = ", ") else "no species",
           "; say which species the count is for.", call. = FALSE)
  }
  new <- new_count(count, type, species, date = date, season = season, accuracy = accuracy,
                   source = source)
  ctx <- count_context(record, new)

  w <- site_window(site, radius_km, lon_0 = lon_0)
  L <- site_map_layers(w, paths)
  map <- site_map_plot(L, places = places, label_sites = label_sites, text_pt = 8.5)

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
                      if (site$source == "MAPPPD") paste0("MAPPPD site ", site$code, ", ") else "",
                      paste0(if (is.null(site$region)) "" else paste0(site$region, ", "),
                             fmt_latlon(site$lon, site$lat)),
                      fmt_n(new$count), new$type, species_phrase(species),
                      if (is.null(new$date)) paste("season", new$season) else fmt_date(new$date),
                      if (is.null(source)) "" else paste0(", ", source))
  context_text <- paste(strwrap(paste(ctx$sentences, collapse = " "),
                                width = if (length(others)) 92 else 80), collapse = "\n")
  caption <- paste0(
    "Colony counts: MAPPPD v", mapppd_version(), " (Humphries et al. 2017), CC BY 4.0; ",
    "each season's peak count is joined, and the error bar is the MAPPPD accuracy class ",
    sprintf("(%d: within %d%%).", new$accuracy, round(100 * ACCURACY_SHARE[new$accuracy])),
    "\nMap: ", radius_km, " km around the site. ", map_caption(L))

  map_h <- 0.78
  map_w <- map_h * height / width
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

  if (is.null(name)) name <- sprintf("%s_report_%d", tolower(site$code), new$season)
  save_figure(fig, name, out_dir, width = width, height = height, dpi = dpi, bg = "white")
  message(paste(ctx$sentences, collapse = "\n"))
  invisible(structure(fig, context = ctx, record = record, count = new, layers = L))
}

# "2.4" from a package version such as 2.4 or 2.4.1
mapppd_version <- function() {
  v <- tryCatch(as.character(utils::packageVersion("mapppdr")), error = function(e) NA)
  if (is.na(v)) return("")
  paste(head(strsplit(v, ".", fixed = TRUE)[[1]], 2), collapse = ".")
}
