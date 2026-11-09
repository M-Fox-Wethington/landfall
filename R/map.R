# --- The site map ---------------------------------------------------------------

#' The layers of a site map, built once
#'
#' @param window A [site_window()].
#' @param paths The list from [landfall_paths()].
#' @return A list with the `window`, `coast`, `contours`, `rock` and `terrain`.
#' @export
site_map_layers <- function(window, paths = landfall_paths()) {
  coast <- load_coast(window, paths)
  list(window = window, coast = coast,
       contours = load_contours(window, paths = paths),
       rock = load_rock(window, paths),
       terrain = build_site_terrain(window, coast, paths = paths))
}

# The names on the map, in one layout pass.
#
# Place names and the names of the other MAPPPD sites used to be two separate
# ggrepel layers, and a layer cannot see the labels of any other, so the two
# sets had nothing stopping them landing on each other. They are one pass now,
# carrying a size, a weight and a color for each row.
#
# The barrier rows carry no label. They stand for the things the layout cannot
# otherwise see: the scale bar, the north arrow and the site markers, none of
# which reaches ggrepel as a box of its own. A name is pushed only by a point
# that falls inside its own box, so they are laid down as a close lattice.
name_layer <- function(named, barrier) {
  d <- named
  if (!is.null(barrier) && nrow(barrier)) {
    d <- rbind(d, sf::st_sf(label = rep("", nrow(barrier)),
                            size = d$size[1], face = "plain", color = MAP_TEXT,
                            geometry = sf::st_geometry(barrier)))
  }
  ggrepel::geom_text_repel(
    data = d, aes(label = .data$label, geometry = .data$geometry),
    stat = "sf_coordinates", size = pt_size(d$size), fontface = d$face,
    color = d$color, bg.color = alpha(MAP_PAPER, 0.85), bg.r = 0.12, seed = 42,
    # Every barrier point counts as an overlap, so a ceiling here would drop
    # the very names the barrier is meant to move rather than moving them.
    max.overlaps = Inf, min.segment.length = 0.2,
    # ggrepel pushes a label away from a point only within point.padding of
    # it, and the default is a millionth of a line, which is why a barrier
    # built without this moves nothing. The force is raised and the pull back
    # to the point eased for the same reason: a name whose own position falls
    # under the scale bar has to be able to leave the bar and draw a leader
    # back, rather than being held where it started.
    point.padding = unit(0.06, "in"), force = 4, force_pull = 0.8,
    max.iter = 60000, max.time = 20,
    segment.color = alpha(COLORS$slate, 0.6), segment.size = 0.3)
}

# The named points of a map as one table: the places given by the caller, then
# the other MAPPPD sites in the window.
named_points <- function(places, neighbors, crs, size_pt) {
  out <- NULL
  if (!is.null(places) && nrow(places)) {
    p <- sf::st_as_sf(places, coords = c("lon", "lat"), crs = 4326) |>
      sf::st_transform(crs)
    type <- if (is.null(places$type)) rep("land", nrow(p)) else places$type
    out <- rbind(out, sf::st_sf(
      label = as.character(places$name), size = size_pt,
      face = ifelse(type == "water", "italic", "bold"), color = MAP_TEXT,
      geometry = sf::st_geometry(p)))
  }
  if (!is.null(neighbors) && nrow(neighbors)) {
    out <- rbind(out, sf::st_sf(
      label = as.character(neighbors$name), size = size_pt - 1, face = "plain",
      color = COLORS$slate, geometry = sf::st_geometry(neighbors)))
  }
  out
}

#' Draw a site map from its layers
#'
#' The layer stack of the BRI neighborhood maps: ocean, relief, ice shelves,
#' contours, rock outcrops, coastline, the other MAPPPD sites in the window,
#' place names, and the site marker with its label, then the graticule, a
#' scale bar and a north arrow.
#'
#' The scale bar and the north arrow are drawn from the panel's own corners and
#' the names are laid out in the map's own coordinates, so neither knows about
#' the other. Give `panel_in` and both are handed to the layout as points that
#' carry no label, which keeps the names off them; see [map_panel_inches()] for
#' how to measure it. Leave it out and the names are laid out as they were
#' before, with nothing stopping one crossing the bar.
#'
#' @param L The list from [site_map_layers()].
#' @param places A data frame with `name`, `lon`, `lat` and optionally `type`
#'   (`"water"` for italic labels), or `NULL`.
#' @param label_sites Mark the other MAPPPD sites within the window.
#' @param max_site_labels The most neighboring sites to label, nearest first.
#' @param text_pt Size of the place labels in points.
#' @param contours Draw the contours.
#' @param panel_in The drawn panel's width and height in inches, from
#'   [map_panel_inches()], or `NULL` to reserve nothing.
#' @param arrow The north arrow glyph, `"dart"` or `"delta"`; see
#'   [north_arrow_circle()].
#' @return A ggplot object, with the attributes `window`, `panel_in` and
#'   `barrier`, which [check_map_labels()] reads.
#' @export
site_map_plot <- function(L, places = NULL, label_sites = TRUE, max_site_labels = 12,
                          text_pt = 9, contours = TRUE, panel_in = NULL,
                          arrow = c("dart", "delta")) {
  arrow <- match.arg(arrow)
  w <- L$window
  site <- site_sf(w$site, w$crs)
  neighbors <- NULL
  if (label_sites) {
    neighbors <- sites_within(w$site, w$radius_km * sqrt(2), crs = w$crs)
    xy <- sf::st_coordinates(neighbors)
    inside <- xy[, 1] > w$xlim[1] & xy[, 1] < w$xlim[2] & xy[, 2] > w$ylim[1] & xy[, 2] < w$ylim[2]
    neighbors <- head(neighbors[inside, ], max_site_labels)
  }

  # What the layout must keep clear of, in the window's own coordinates: the
  # two pieces of furniture, and the markers, which are drawn in layers of
  # their own and so are invisible to ggrepel.
  barrier <- NULL
  if (!is.null(panel_in) && length(panel_in) == 2 && all(is.finite(panel_in))) {
    barrier <- rbind(
      lattice_in_window(furniture_boxes(panel_in), w, panel_in),
      marker_barrier(site, marker_radius_in(3.8 + 1.8), w, panel_in),
      marker_barrier(neighbors, marker_radius_in(2.6), w, panel_in))
  }

  p <- ggplot() +
    annotate("rect", xmin = w$xlim[1] - w$pad_m, xmax = w$xlim[2] + w$pad_m,
             ymin = w$ylim[1] - w$pad_m, ymax = w$ylim[2] + w$pad_m, fill = MAP_OCEAN) +
    geom_spatraster_rgb(data = L$terrain$rgb, maxcell = 5e6, interpolate = TRUE)
  if (nrow(L$coast$shelves)) {
    p <- p + geom_sf(data = L$coast$shelves, fill = alpha(SHELF_FILL, 0.50),
                     color = SHELF_BORDER, linewidth = 0.18)
  }
  if (contours && !is.null(L$contours)) {
    p <- p +
      geom_sf(data = L$contours[!L$contours$is_major, ], color = alpha(MAP_COAST, 0.20),
              linewidth = 0.10) +
      geom_sf(data = L$contours[L$contours$is_major, ], color = alpha(MAP_COAST, 0.45),
              linewidth = 0.18)
  }
  if (!is.null(L$rock)) {
    p <- p + geom_sf(data = L$rock, fill = alpha(ROCK_FILL, 0.78),
                     color = alpha(ROCK_BORDER, 0.55), linewidth = 0.10)
  }
  if (nrow(L$coast$land)) {
    p <- p + geom_sf(data = L$coast$land, fill = NA, color = MAP_COAST, linewidth = 0.30)
  }
  if (!is.null(neighbors) && nrow(neighbors)) {
    p <- p +
      geom_sf(data = neighbors, color = "white", size = 2.6, shape = 16) +
      geom_sf(data = neighbors, color = COLORS$slate, size = 1.6, shape = 16)
  }
  named <- named_points(places, neighbors, w$crs, text_pt)
  if (!is.null(named) && nrow(named)) p <- p + name_layer(named, barrier)
  p <- p +
    geom_halo_points(site, color = SITE_COLOR, size = 3.8) +
    # The site's own label is a boxed one and cannot share the pass above. It
    # sits near the middle of the window with nothing to push it, so it does
    # not reach either corner; check_map_labels() reports it if it ever does.
    ggrepel::geom_label_repel(
      data = site, aes(label = .data$label, geometry = .data$geometry),
      stat = "sf_coordinates", size = pt_size(text_pt + 1), fontface = "bold",
      color = COLORS$title, fill = alpha(MAP_PAPER, 0.92), label.size = 0.32,
      label.r = unit(0.10, "lines"), nudge_x = 0.18 * w$radius_km * 1000,
      nudge_y = -0.09 * w$radius_km * 1000, segment.color = COLORS$title,
      segment.size = 0.5, seed = 42, min.segment.length = 0) +
    coord_sf(crs = w$crs, xlim = w$xlim, ylim = w$ylim, expand = FALSE,
             datum = sf::st_crs(4326)) +
    map_furniture(arrow) +
    theme_site_map()
  with_map_attrs(p, w, panel_in, barrier)
}

# Adding a layer or a theme to a ggplot can return a new object, so the
# attributes that check_map_labels() reads are set here and set again after
# anything is added.
with_map_attrs <- function(p, window, panel_in, barrier) {
  attr(p, "window") <- window
  attr(p, "panel_in") <- panel_in
  attr(p, "barrier") <- barrier
  p
}

# The map theme of the neighborhood maps: parchment page, ocean panel, slate
# neatline and graticule, edge labels in the coast color.
theme_site_map <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = MAP_PAPER, color = NA),
      panel.background = element_rect(fill = MAP_OCEAN, color = NA),
      panel.border     = element_rect(color = MAP_COAST, fill = NA, linewidth = 0.5),
      panel.grid.major = element_line(color = alpha(MAP_COAST, 0.22), linewidth = 0.25),
      panel.grid.minor = element_blank(),
      axis.text  = element_text(size = 7, color = MAP_COAST),
      axis.ticks = element_line(color = alpha(MAP_COAST, 0.6), linewidth = 0.3),
      axis.ticks.length = unit(2, "pt"),
      axis.title = element_blank(),
      plot.margin = margin(4, 4, 4, 4),
      legend.position = "none"
    )
}

# What a standalone site map adds on top of the map theme. The panel
# measurement needs it too, because a title and a caption take height the
# panel would otherwise have.
theme_site_map_titled <- function() {
  theme(plot.title = element_text(face = "bold", size = 16, color = MAP_TEXT, hjust = 0.5,
                                  margin = margin(t = 8, b = 4)),
        plot.caption = element_text(size = 7, color = COLORS$muted, hjust = 0,
                                    lineheight = 1.3, margin = margin(t = 8)),
        plot.margin = margin(12, 12, 10, 12))
}

# The source line under a map.
map_caption <- function(L) {
  layers <- c("coastline", if (nrow(L$coast$shelves)) "ice shelves",
              if (!is.null(L$contours)) "contours", if (!is.null(L$rock)) "rock outcrops")
  layers <- if (length(layers) > 1) {
    paste0(paste(head(layers, -1), collapse = ", "), " and ", tail(layers, 1))
  } else layers
  layers <- paste0(toupper(substr(layers, 1, 1)), substr(layers, 2, nchar(layers)))
  if (!is.null(L$contours) && nrow(L$contours)) {
    lv <- sort(unique(L$contours$level))
    if (length(lv) > 1) {
      layers <- sub("ontours", sprintf("ontours at %g m", min(diff(lv))), layers)
    }
  }
  # Shaded relief is not a height scale, so the caption says what the heights
  # are measured from. REMA reads from the WGS84 ellipsoid, and the relief
  # takes off the sea surface measured in this window when there was enough
  # water to measure one.
  sea <- L$terrain$sea_level
  datum <- if (is.null(sea) || is.na(sea) || sea == 0) {
    "heights on the WGS84 ellipsoid"
  } else {
    sprintf("heights above the local sea surface, %.1f m on the WGS84 ellipsoid", sea)
  }
  # The tint is stretched to this window's own relief, so what it runs to is
  # part of reading it and a color here does not mean the same height on
  # another map.
  top <- L$terrain$ramp_max
  tint <- if (is.null(top) || is.na(top)) "" else {
    sprintf(" Land is tinted from the sea to %.0f m, stretched to this window.",
            top)
  }
  sprintf(paste("Relief: REMA v2.0 %s mosaic (Howat et al.), Polar Geospatial Center,",
                "%g m grid; %s.%s %s: SCAR Antarctic Digital Database (%s)."),
          L$terrain$source, L$terrain$grid_res, datum, tint, layers, L$coast$source)
}

#' Figure: the map of a site's surroundings
#'
#' A square relief map of `radius_km` around the site, with the other MAPPPD
#' sites in the window marked, written as a PNG and a PDF when `out_dir` is
#' set.
#'
#' @param site A site, as [as_site()] accepts: a four-letter MAPPPD code, or
#'   the result of [site_lookup()] or [site_point()].
#' @param radius_km Half-width of the map in kilometers.
#' @param lon_0 Central meridian of the projection, or `NULL` for the site's
#'   own.
#' @param title Figure title, or `NULL` for the site's name and the radius.
#' @param out_dir Folder to write into, or `NULL` to write nothing.
#' @param name File name stem.
#' @param overwrite Write over files of the same name that already exist.
#' @param width,height Size in inches.
#' @param dpi Resolution of the PNG.
#' @param paths The list from [landfall_paths()].
#' @inheritParams site_map_plot
#' @return The ggplot object, invisibly, with attribute `map_layers`, the
#'   list from [site_map_layers()].
#' @export
site_map <- function(site, radius_km = 25, lon_0 = NULL, places = NULL, label_sites = TRUE,
                     arrow = c("dart", "delta"), title = NULL,
                     out_dir = landfall_paths()$output, name = NULL,
                     overwrite = FALSE, width = 12, height = 12, dpi = 300,
                     paths = landfall_paths()) {
  site <- as_site(site)
  w <- site_window(site, radius_km, lon_0 = lon_0)
  L <- site_map_layers(w, paths)
  if (is.null(title)) title <- sprintf("%s, %g km around the site", site$name, radius_km)
  # A plot caption does not wrap, so it is wrapped here and the panel is then
  # measured with it, because every line it takes is height the panel loses.
  # About twenty characters of 7 point type fit in an inch, and half an inch
  # is left for the margins.
  caption <- paste(strwrap(map_caption(L), width = max(60, floor(20 * (width - 0.5)))),
                   collapse = "
")
  panel_in <- map_panel_inches(w, width, height, title = title, caption = caption,
                               theme_extra = theme_site_map_titled())
  p <- site_map_plot(L, places = places, label_sites = label_sites,
                     panel_in = panel_in, arrow = arrow)
  barrier <- attr(p, "barrier")
  p <- p + labs(title = title, caption = caption) + theme_site_map_titled()
  p <- with_map_attrs(p, w, panel_in, barrier)
  if (is.null(name)) name <- sprintf("%s_map_%gkm", tolower(site$code), radius_km)
  save_figure(p, name, out_dir, width = width, height = height, dpi = dpi, bg = MAP_PAPER,
              overwrite = overwrite)
  # The attribute is map_layers and not layers, because ggplot2 4.0 keeps a
  # plot's own properties in its attributes, so an attribute called layers
  # replaces the plot's layers and returns a figure that cannot be drawn.
  attr(p, "map_layers") <- L
  invisible(p)
}
