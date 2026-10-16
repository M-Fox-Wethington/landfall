# --- The colony trend panel --------------------------------------------------------

#' Panel: a species' counts at a site, season by season
#'
#' Every MAPPPD count of the species at the site is drawn as a small point,
#' with the chosen count type in slate and the other types in gray. A line
#' joins the peak count of each season for the chosen type, and the new count,
#' when given, sits on the series in orange with its accuracy range as an
#' error bar.
#'
#' @param record The table from [colony_record()].
#' @param species A species code.
#' @param type The count type the series follows; defaults to the new count's.
#' @param new A [new_count()], or `NULL`.
#' @param base_size Base font size in points.
#' @param title Panel title, or `NULL` for the species name.
#' @return A ggplot object.
#' @export
panel_colony_trend <- function(record, species, type = NULL, new = NULL, base_size = 11,
                               title = NULL) {
  if (is.null(type)) type <- if (is.null(new)) "nests" else new$type
  d <- record[record$species_id == species, ]
  d$focal <- d$type == type
  peaks <- season_peaks(record, species, type)
  x_min <- min(c(d$season, if (!is.null(new)) new$season, 1980), na.rm = TRUE)
  x_max <- max(c(d$season, if (!is.null(new)) new$season), na.rm = TRUE) + 1
  y_max <- max(c(d$count, if (!is.null(new)) new$hi), na.rm = TRUE)
  shapes <- c(nests = 16, chicks = 17, adults = 15)
  type_word <- c(nests = "Nests", chicks = "Chicks", adults = "Adults")[[type]]

  p <- ggplot() +
    geom_line(data = peaks, aes(season, peak), color = COLORS$slate, linewidth = 0.7) +
    geom_point(data = d[!d$focal, ], aes(season, count, shape = type), color = COLORS$muted,
               size = 2.0, alpha = 0.9) +
    geom_point(data = d[d$focal, ], aes(season, count, shape = type), color = COLORS$slate,
               size = 2.0, alpha = 0.55) +
    geom_point(data = peaks, aes(season, peak), shape = shapes[[type]], color = COLORS$slate,
               size = 2.8)
  if (!is.null(new)) {
    nd <- data.frame(season = new$season, count = new$count, lo = new$lo, hi = new$hi)
    p <- p +
      geom_errorbar(data = nd, aes(season, ymin = lo, ymax = hi), width = 0.9,
                    linewidth = 0.6, color = COLORS$orange) +
      geom_point(data = nd, aes(season, count), size = 7.0, color = "white") +
      geom_point(data = nd, aes(season, count), size = 5.0, color = COLORS$orange) +
      geom_point(data = nd, aes(season, count), size = 5.0, shape = 1, stroke = 0.6,
                 color = alpha(COLORS$title, 0.45)) +
      ggrepel::geom_text_repel(
        data = nd, aes(season, count, label = sprintf("%d: %s %s", season, fmt_n(count), type)),
        size = pt(base_size), fontface = "bold", color = COLORS$orange, nudge_y = 0.08 * y_max,
        segment.color = COLORS$orange, segment.size = 0.4, min.segment.length = 0.1,
        box.padding = 0.6, point.padding = 0.6, seed = 42)
  }
  n_types <- length(unique(d$type))
  p +
    scale_shape_manual(values = shapes, breaks = names(shapes), name = NULL, drop = TRUE) +
    scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.10))) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 8), limits = c(x_min, x_max)) +
    coord_cartesian(clip = "off") +
    labs(title = if (is.null(title)) species_label(species) else title,
         subtitle = sprintf("%s counted each season, MAPPPD%s", type_word,
                            if (is.null(new)) "" else "; the new count in orange"),
         x = NULL, y = sprintf("%s counted", type_word)) +
    theme_bri(base_size) +
    theme(legend.position = if (n_types > 1) "bottom" else "none",
          legend.margin = margin(t = 0),
          plot.title = element_text(size = base_size * 1.3),
          plot.subtitle = element_text(size = base_size * 0.9),
          axis.text = element_text(size = base_size * 0.85),
          axis.title = element_text(size = base_size * 0.9))
}
