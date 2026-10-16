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

# Place labels: bold for land, italic for water, on a faint parchment halo.
place_layers <- function(places, crs, size_pt = 9) {
  if (is.null(places) || !nrow(places)) return(NULL)
  p <- sf::st_as_sf(places, coords = c("lon", "lat"), crs = 4326) |> sf::st_transform(crs)
  if (is.null(p$type)) p$type <- "land"
  ggrepel::geom_text_repel(
    data = p, aes(label = .data$name, geometry = .data$geometry,
                  fontface = ifelse(.data$type == "water", "italic", "bold")),
    stat = "sf_coordinates", size = pt(size_pt), color = MAP_TEXT,
    bg.color = alpha(MAP_PAPER, 0.85), bg.r = 0.12, seed = 42, max.overlaps = 12,
    min.segment.length = 0.3)
}

# Other MAPPPD sites in the window, as small markers with their names.
neighbor_layers <- function(neighbors, size_pt = 8) {
  if (is.null(neighbors) || !nrow(neighbors)) return(NULL)
  list(
    geom_sf(data = neighbors, color = "white", size = 2.6, shape = 16),
    geom_sf(data = neighbors, color = COLORS$slate, size = 1.6, shape = 16),
    ggrepel::geom_text_repel(
      data = neighbors, aes(label = .data$name, geometry = .data$geometry),
      stat = "sf_coordinates", size = pt(size_pt), color = COLORS$slate,
      bg.color = alpha(MAP_PAPER, 0.85), bg.r = 0.1, seed = 42, max.overlaps = Inf,
      min.segment.length = 0.2, segment.color = alpha(COLORS$slate, 0.6),
      segment.size = 0.3)
  )
}

#' Draw a site map from its layers
#'
#' The layer stack of the BRI neighborhood maps: ocean, relief, ice shelves,
#' contours, rock outcrops, coastline, the other MAPPPD sites in the window,
#' place names, and the site marker with its label, then the graticule, a
#' scale bar and a north arrow.
#'
#' @param L The list from [site_map_layers()].
#' @param places A data frame with `name`, `lon`, `lat` and optionally `type`
#'   (`"water"` for italic labels), or `NULL`.
#' @param label_sites Mark the other MAPPPD sites within the window.
#' @param max_site_labels The most neighboring sites to label, nearest first.
#' @param text_pt Size of the place labels in points.
#' @param contours Draw the contours.
#' @return A ggplot object.
#' @export
site_map_plot <- function(L, places = NULL, label_sites = TRUE, max_site_labels = 12,
                          text_pt = 9, contours = TRUE) {
  w <- L$window
  site <- site_sf(w$site, w$crs)
  neighbors <- NULL
  if (label_sites) {
    neighbors <- sites_within(w$site, w$radius_km * sqrt(2), crs = w$crs)
    xy <- sf::st_coordinates(neighbors)
    inside <- xy[, 1] > w$xlim[1] & xy[, 1] < w$xlim[2] & xy[, 2] > w$ylim[1] & xy[, 2] < w$ylim[2]
    neighbors <- head(neighbors[inside, ], max_site_labels)
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
  p <- p +
    neighbor_layers(neighbors, size_pt = text_pt - 1) +
    place_layers(places, w$crs, size_pt = text_pt) +
    geom_halo_points(site, color = SITE_COLOR, size = 3.8) +
    ggrepel::geom_label_repel(
      data = site, aes(label = .data$label, geometry = .data$geometry),
      stat = "sf_coordinates", size = pt(text_pt + 1), fontface = "bold",
      color = COLORS$title, fill = alpha(MAP_PAPER, 0.92), label.size = 0.32,
      label.r = unit(0.10, "lines"), nudge_x = 0.18 * w$radius_km * 1000,
      nudge_y = -0.09 * w$radius_km * 1000, segment.color = COLORS$title,
      segment.size = 0.5, seed = 42, min.segment.length = 0) +
    coord_sf(crs = w$crs, xlim = w$xlim, ylim = w$ylim, expand = FALSE,
             datum = sf::st_crs(4326)) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.22, style = "ticks",
                                text_col = MAP_COAST, line_col = MAP_COAST,
                                pad_x = unit(0.8, "cm"), pad_y = unit(0.8, "cm"),
                                text_cex = 0.75) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      style = ggspatial::north_arrow_minimal(line_col = MAP_COAST, text_col = MAP_COAST,
                                             fill = MAP_COAST),
      height = unit(1.0, "cm"), width = unit(0.7, "cm")) +
    theme_site_map()
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

# The source line under a map.
map_caption <- function(L) {
  layers <- c("coastline", if (nrow(L$coast$shelves)) "ice shelves",
              if (!is.null(L$contours)) "contours", if (!is.null(L$rock)) "rock outcrops")
  layers <- if (length(layers) > 1) {
    paste0(paste(head(layers, -1), collapse = ", "), " and ", tail(layers, 1))
  } else layers
  layers <- paste0(toupper(substr(layers, 1, 1)), substr(layers, 2, nchar(layers)))
  sprintf("Relief: REMA v2.0 %s mosaic (Howat et al.), Polar Geospatial Center. %s: SCAR Antarctic Digital Database (%s).",
          L$terrain$source, layers, L$coast$source)
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
#' @param width,height Size in inches.
#' @param dpi Resolution of the PNG.
#' @param paths The list from [landfall_paths()].
#' @inheritParams site_map_plot
#' @return The ggplot object, invisibly, with attribute `layers`.
#' @export
site_map <- function(site, radius_km = 25, lon_0 = NULL, places = NULL, label_sites = TRUE,
                     title = NULL, out_dir = landfall_paths()$output, name = NULL,
                     width = 12, height = 12, dpi = 300, paths = landfall_paths()) {
  site <- as_site(site)
  w <- site_window(site, radius_km, lon_0 = lon_0)
  L <- site_map_layers(w, paths)
  if (is.null(title)) title <- sprintf("%s, %g km around the site", site$name, radius_km)
  p <- site_map_plot(L, places = places, label_sites = label_sites) +
    labs(title = title, caption = map_caption(L)) +
    theme(plot.title = element_text(face = "bold", size = 16, color = MAP_TEXT, hjust = 0.5,
                                    margin = margin(t = 8, b = 4)),
          plot.caption = element_text(size = 7, color = COLORS$muted, hjust = 0,
                                      lineheight = 1.3, margin = margin(t = 8)),
          plot.margin = margin(12, 12, 10, 12))
  if (is.null(name)) name <- sprintf("%s_map_%gkm", tolower(site$code), radius_km)
  save_figure(p, name, out_dir, width = width, height = height, dpi = dpi, bg = MAP_PAPER)
  invisible(structure(p, layers = L))
}
