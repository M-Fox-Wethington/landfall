# --- The colony trend panel --------------------------------------------------------

#' Panel: a species' counts at a site, season by season
#'
#' Every MAPPPD count of the species at the site is drawn as a small point,
#' with the chosen count type in slate and the other types in gray. A line
#' joins the highest recorded count of each season for the chosen type, and the
#' new count, when given, sits on the series in orange with the range of its
#' accuracy class as a bar. A count whose class was not recorded is drawn
#' without a bar.
#'
#' Nests, chicks and adults are different quantities, so when more than one
#' type is present the panel says so on its axis and separates the types by
#' shape.
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
  species <- toupper(species)
  d <- record[record$species_id == species, ]
  d$focal <- d$type == type
  peaks <- season_peaks(record, species, type)
  x_min <- min(c(d$season, if (!is.null(new)) new$season, 1980), na.rm = TRUE)
  x_max <- max(c(d$season, if (!is.null(new)) new$season), na.rm = TRUE) + 1
  y_max <- max(c(d$count, if (!is.null(new)) c(new$count, new$hi)), na.rm = TRUE)
  shapes <- c(nests = 16, chicks = 17, adults = 15)
  type_word <- c(nests = "Nests", chicks = "Chicks", adults = "Adults")[[type]]
  has_range <- !is.null(new) && !is.na(new$lo) && !is.na(new$hi)

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
    if (has_range) {
      p <- p + geom_errorbar(data = nd, aes(season, ymin = lo, ymax = hi), width = 0.9,
                             linewidth = 0.6, color = COLORS$orange)
    }
    p <- p +
      # The leader goes down first, so the marker's white halo cuts it off at
      # the circle's edge rather than the line being drawn across the circle.
      ggrepel::geom_text_repel(
        data = nd, aes(season, count, label = sprintf("%d: %s %s", season, fmt_n(count), type)),
        size = pt_size(base_size), fontface = "bold", color = COLORS$orange,
        nudge_y = 0.08 * y_max, segment.color = COLORS$orange, segment.size = 0.4,
        min.segment.length = 0.1, box.padding = 0.6, point.padding = 0.6, seed = 42) +
      geom_point(data = nd, aes(season, count), size = 7.0, color = "white") +
      geom_point(data = nd, aes(season, count), size = 5.0, color = COLORS$orange) +
      geom_point(data = nd, aes(season, count), size = 5.0, shape = 1, stroke = 0.6,
                 color = alpha(COLORS$title, 0.45))
  }
  types_present <- intersect(names(shapes), unique(d$type))
  n_types <- length(types_present)
  # Nests, chicks and adults do not share a meaning, so an axis carrying more
  # than one of them is named for all of them.
  y_lab <- if (n_types > 1) {
    sprintf("%s or %s counted", paste(utils::head(types_present, -1), collapse = ", "),
            utils::tail(types_present, 1))
  } else {
    sprintf("%s counted", type_word)
  }
  # The panel sits beside a map in a 16 inch figure, so the subtitle has to fit
  # in its own width; what the bar on the new count means is said in the
  # figure's caption.
  one <- c(nests = "nest", chicks = "chick", adults = "adult")[[type]]
  subtitle <- sprintf("Highest recorded %s count of each season, MAPPPD%s", one,
                      if (is.null(new)) "" else "; the new count in orange")
  p +
    scale_shape_manual(values = shapes, breaks = names(shapes), name = NULL, drop = TRUE) +
    scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.10))) +
    scale_x_continuous(breaks = scales::breaks_pretty(n = 8), limits = c(x_min, x_max)) +
    coord_cartesian(clip = "off") +
    labs(title = if (is.null(title)) species_label(species) else title,
         subtitle = subtitle, x = NULL, y = y_lab) +
    theme_bri(base_size) +
    theme(legend.position = if (n_types > 1) "bottom" else "none",
          legend.margin = margin(t = 0),
          plot.title = element_text(size = base_size * 1.3),
          plot.subtitle = element_text(size = base_size * 0.9),
          axis.text = element_text(size = base_size * 0.85),
          axis.title = element_text(size = base_size * 0.9))
}
