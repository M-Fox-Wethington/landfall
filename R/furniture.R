# --- Map furniture and the label layout ----------------------------------------
#
# ggspatial draws the scale bar and the north arrow from the panel's own
# corners, in centimeters, and ggrepel lays out names in the data's own
# coordinates. Neither knows about the other, so a name can be set straight
# across the bar or the arrow and nothing reports it. The repair is to hand
# each piece of furniture back to the layout as a grid of points that carry no
# label, because a name is pushed only by a point that falls inside its own
# box, and a point carrying no label draws nothing.
#
# Turning a corner measured in centimeters into a rectangle in meters of ground
# needs the drawn panel's size, which is the figure's size less its margins,
# its title, its caption and its axis labels. That is measured rather than
# estimated: map_panel_inches() draws a plot carrying the same frame, theme and
# text but no data layers, and reads the panel's size off the result. The
# skeleton and the full map were confirmed to give the same panel to five
# decimal places at three figure sizes.

CM_PER_IN <- 2.54

# The two annotations' own settings, in one place, so the reservation here and
# the annotations drawn in site_map_plot() cannot drift apart. Everything is
# passed explicitly there, including the arrow's padding, so none of this
# depends on a default that a later ggspatial could change.
SCALE_PAD_CM     <- 0.8
SCALE_WIDTH_HINT <- 0.22
ARROW_PAD_CM     <- 0.25
# The circled arrow needs a square box, because the ring is drawn inside a
# square viewport centered in whatever box it is given.
ARROW_W_CM       <- 1.2
ARROW_H_CM       <- 1.2
# The height of the bar itself, which its own numbers then sit above.
SCALE_BAR_IN     <- 0.10

# How far past the end of the bar its own numbers run, and how tall the bar
# and its numbers are together. Both were measured by drawing the map twice,
# once with map_furniture() and once without, and reading the bounding box of
# every pixel that changed. The last number sits past the end of the bar on
# the same line, which is why the block is wide and short: it ran 0.23 inches
# past the bar on an 11.50 inch panel and stood 0.115 inches tall there and on
# a 6.52 inch panel. The two figures below leave room for a longer number such
# as "100 km".
SCALE_LABEL_IN <- 0.45
SCALE_BLOCK_IN <- 0.22

# The scale bar and the north arrow, built here rather than in the map, so the
# rectangles reserved above and the things actually drawn cannot drift apart.
# The measurement that set those rectangles draws a map with and without this.
map_furniture <- function(arrow = c("dart", "delta")) {
  arrow <- match.arg(arrow)
  list(
    # A filled alternating bar, not ticks. Tick marks in the coastline blue
    # sit at about 1.6 to 1 against this ocean and disappear into the relief,
    # which is the same reason the Antarctic Sound sheet uses a bar.
    ggspatial::annotation_scale(
      location = "bl", width_hint = SCALE_WIDTH_HINT, style = "bar",
      bar_cols = c(COLORS$title, "white"), text_col = COLORS$title,
      line_col = COLORS$title, text_cex = 0.8, line_width = 0.8,
      height = grid::unit(SCALE_BAR_IN, "in"),
      pad_x = grid::unit(SCALE_PAD_CM, "cm"),
      pad_y = grid::unit(SCALE_PAD_CM, "cm")),
    # The padding is passed rather than left to ggspatial's own default,
    # because the reservation above is measured from these same numbers.
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      style = north_arrow_circle(arrow, halo = ggplot2::alpha(MAP_PAPER, 0.85),
                                 line_col = MAP_COAST, fill = MAP_COAST,
                                 bg = MAP_PAPER, text_col = MAP_COAST,
                                 text_size = 11),
      pad_x = grid::unit(ARROW_PAD_CM, "cm"),
      pad_y = grid::unit(ARROW_PAD_CM, "cm"),
      height = grid::unit(ARROW_H_CM, "cm"),
      width = grid::unit(ARROW_W_CM, "cm")))
}

#' The drawn size of a map panel, in inches
#'
#' The panel is the mapped area inside the axis labels, which is the figure
#' less its margins, its title, its caption and the space the edge labels take.
#' It is measured by drawing a plot that carries the same frame, theme and text
#' as the map but none of its layers, because the panel's size does not depend
#' on what is drawn inside it. A square window in a projected coordinate system
#' gives a square panel.
#'
#' @param window A [site_window()].
#' @param width,height Size of the figure, or of the slot the map is drawn
#'   into, in inches.
#' @param title Title the map will carry, or `NULL` for none.
#' @param caption Caption the map will carry, or `NULL` for none.
#' @param theme_extra Extra theme applied on top of the map theme, or `NULL`.
#' @return The panel's width and height in inches, or `c(NA, NA)` when the
#'   panel could not be read.
#' @export
map_panel_inches <- function(window, width, height, title = NULL, caption = NULL,
                             theme_extra = NULL) {
  p <- ggplot() +
    coord_sf(crs = window$crs, xlim = window$xlim, ylim = window$ylim,
             expand = FALSE, datum = sf::st_crs(4326)) +
    labs(title = title, caption = caption) +
    theme_site_map()
  if (!is.null(theme_extra)) p <- p + theme_extra
  measure_panel(p, width, height)
}

# Draw a plot on a throwaway device and read the panel viewport's size. The
# device is a temporary PDF, so nothing reaches the screen or the output
# folder, and a failure to read the panel returns NA rather than stopping the
# figure that asked for it.
measure_panel <- function(p, width, height) {
  tryCatch({
    f <- tempfile(fileext = ".pdf")
    grDevices::pdf(f, width = width, height = height)
    on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
    grid::grid.newpage()
    print(p)
    grid::grid.force()
    vp <- panel_viewport()
    if (!length(vp)) return(c(NA_real_, NA_real_))
    grid::seekViewport(vp[1])
    c(grid::convertWidth(grid::unit(1, "npc"), "in", TRUE),
      grid::convertHeight(grid::unit(1, "npc"), "in", TRUE))
  }, error = function(e) c(NA_real_, NA_real_))
}

# The names of the panel viewports on the device, in drawing order. ggplot2
# names a panel viewport "panel." followed by its cell in the layout.
panel_viewport <- function() {
  l <- grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
  nm <- unlist(l$name)
  nm[startsWith(nm, "panel.")]
}

#' Where the scale bar and the north arrow sit in a panel
#'
#' The two rectangles the annotations occupy, in inches measured from the
#' panel's bottom left corner. Each is the annotation's own footprint plus a
#' margin, so a name is kept clear of the furniture and not merely off it.
#'
#' @param panel_in The panel's width and height in inches, from
#'   [map_panel_inches()].
#' @param margin_in How far beyond the furniture to reserve, in inches.
#' @return A data frame with one row per piece of furniture and the columns
#'   `what`, `x1`, `x2`, `y1` and `y2`.
#' @export
furniture_boxes <- function(panel_in, margin_in = 0.06) {
  stopifnot(length(panel_in) == 2, all(is.finite(panel_in)))
  pw <- panel_in[1]; ph <- panel_in[2]
  m <- margin_in
  scale_pad <- SCALE_PAD_CM / CM_PER_IN
  arrow_pad <- ARROW_PAD_CM / CM_PER_IN
  arrow_w <- ARROW_W_CM / CM_PER_IN
  arrow_h <- ARROW_H_CM / CM_PER_IN
  rbind(
    # The bar runs right from the bottom left corner. Its drawn length is a
    # round number of kilometers no longer than the width hint, so reserving
    # the hint in full is an upper bound whatever number it settles on.
    data.frame(what = "scale bar",
               x1 = scale_pad - m,
               x2 = scale_pad + SCALE_WIDTH_HINT * pw + SCALE_LABEL_IN + m,
               y1 = scale_pad - m,
               y2 = scale_pad + SCALE_BLOCK_IN + m),
    # The arrow is a narrow glyph centered in its own box at the top right.
    data.frame(what = "north arrow",
               x1 = pw - arrow_pad - arrow_w - m,
               x2 = pw - arrow_pad + m,
               y1 = ph - arrow_pad - arrow_h - m,
               y2 = ph - arrow_pad + m)
  )
}

# A lattice of points covering a rectangle given in inches, returned in the
# window's own coordinates. The spacing is closer than the shortest label box
# is tall, because a label is pushed only by a point inside its own box, so a
# lattice any coarser could be straddled.
lattice_in_window <- function(boxes, window, panel_in, step_in = 0.08) {
  mx <- diff(window$xlim) / panel_in[1]
  my <- diff(window$ylim) / panel_in[2]
  d <- do.call(rbind, lapply(seq_len(nrow(boxes)), function(i) {
    expand.grid(x = seq(boxes$x1[i], boxes$x2[i], by = step_in),
                y = seq(boxes$y1[i], boxes$y2[i], by = step_in))
  }))
  if (is.null(d) || !nrow(d)) return(NULL)
  sf::st_as_sf(data.frame(x = window$xlim[1] + d$x * mx,
                          y = window$ylim[1] + d$y * my),
               coords = c("x", "y"), crs = window$crs)
}

# Filled discs of points under the site and neighbor markers, in the window's
# own coordinates. Rings alone are not enough: a label well inside a disc
# touches nothing on its edge, so the disc is filled from its drawn radius
# inward and a point is placed at the middle.
marker_barrier <- function(points, radius_in, window, panel_in, step_in = 0.08) {
  if (is.null(points) || !nrow(points)) return(NULL)
  m_per_in <- diff(window$xlim) / panel_in[1]
  r <- radius_in * m_per_in
  step <- step_in * m_per_in
  xy <- sf::st_coordinates(points)
  d <- do.call(rbind, lapply(seq_len(nrow(xy)), function(i) {
    rs <- seq(r, 0, length.out = max(2, 1 + ceiling(r / step)))
    do.call(rbind, lapply(rs, function(rr) {
      if (rr < step / 2) return(data.frame(x = xy[i, 1], y = xy[i, 2]))
      n <- max(8, ceiling(2 * pi * rr / step))
      th <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)]
      data.frame(x = xy[i, 1] + rr * cos(th), y = xy[i, 2] + rr * sin(th))
    }))
  }))
  sf::st_as_sf(d, coords = c("x", "y"), crs = window$crs)
}

# The radius a point marker draws at, in inches. ggplot2 sizes a point in
# millimeters of diameter, so the radius is half of that converted to inches.
marker_radius_in <- function(size) size * ggplot2::.pt / 2 / 72

#' Measure where a map's labels landed
#'
#' Draws the map on a throwaway device and reads every label back out of the
#' drawing, so what is reported is where the names actually went and not where
#' they were asked to go. Two things are checked. Two label boxes that meet are
#' an overlap a reader will see. A barrier point inside a label box means the
#' layout did not clear whatever that point stands for, which is the scale bar,
#' the north arrow or a site marker, and is the only way to catch those,
#' because none of them reaches the layout as a box of its own.
#'
#' @param p A map from [site_map_plot()].
#' @param width,height The size the map will be drawn at, in inches.
#' @param graze_in How far inside a box a barrier point must sit before it
#'   counts as more than a clipped corner.
#' @return A list with `boxes` (one row per label, in inches from the panel's
#'   bottom left corner), `overlaps` (one row per pair of boxes that meet) and
#'   `on_barrier` (one row per label sitting on a barrier point), or `NULL`
#'   when the drawing could not be read.
#' @export
check_map_labels <- function(p, width, height, graze_in = 0.02) {
  boxes <- tryCatch(label_boxes(p, width, height), error = function(e) {
    message("The labels could not be measured, and the map is unaffected: ",
            conditionMessage(e))
    NULL
  })
  if (is.null(boxes) || !nrow(boxes)) return(NULL)

  o <- NULL
  if (nrow(boxes) > 1) {
    for (i in seq_len(nrow(boxes) - 1)) {
      j <- seq.int(i + 1, nrow(boxes))
      ax <- pmin(boxes$x2[i], boxes$x2[j]) - pmax(boxes$x1[i], boxes$x1[j])
      ay <- pmin(boxes$y2[i], boxes$y2[j]) - pmax(boxes$y1[i], boxes$y1[j])
      k <- which(ax > 0 & ay > 0)
      if (length(k)) {
        o <- rbind(o, data.frame(a = boxes$label[i], b = boxes$label[j][k],
                                 w = ax[k], h = ay[k], stringsAsFactors = FALSE))
      }
    }
  }

  hits <- NULL
  bar <- attr(p, "barrier")
  w <- attr(p, "window")
  panel <- attr(p, "panel_in")
  if (!is.null(bar) && nrow(bar) && !is.null(w) && !is.null(panel)) {
    xy <- sf::st_coordinates(bar)
    # grid measures a viewport up from its floor, and the boxes above were read
    # the same way, so these are inches up from the panel's bottom edge and not
    # down from its top.
    bx <- (xy[, 1] - w$xlim[1]) / (diff(w$xlim) / panel[1])
    by <- (xy[, 2] - w$ylim[1]) / (diff(w$ylim) / panel[2])
    for (i in seq_len(nrow(boxes))) {
      inside <- bx >= boxes$x1[i] & bx <= boxes$x2[i] &
        by >= boxes$y1[i] & by <= boxes$y2[i]
      if (!any(inside)) next
      # A count alone cannot tell a label sitting squarely on the bar from one
      # whose corner clips a single point, so the depth is measured too: how
      # far inside the box the deepest point sits.
      deep <- max(pmin(bx[inside] - boxes$x1[i], boxes$x2[i] - bx[inside],
                       by[inside] - boxes$y1[i], boxes$y2[i] - by[inside]))
      hits <- rbind(hits, data.frame(label = boxes$label[i], n = sum(inside),
                                     depth_in = deep, stringsAsFactors = FALSE))
    }
    if (!is.null(hits)) hits <- hits[order(-hits$depth_in), ]
  }

  # An empty result is a table with no rows and not NULL, so a caller can
  # always read nrow() off it.
  no_overlaps <- data.frame(a = character(0), b = character(0),
                            w = numeric(0), h = numeric(0),
                            stringsAsFactors = FALSE)
  no_hits <- data.frame(label = character(0), n = integer(0), depth_in = numeric(0),
                        stringsAsFactors = FALSE)
  out <- list(boxes = boxes,
              overlaps = if (is.null(o)) no_overlaps else o[order(-pmin(o$w, o$h)), ],
              on_barrier = if (is.null(hits)) no_hits else hits)
  message(sprintf("%d labels drawn. Boxes that meet: %d. Labels on a barrier point: %d, of which %d deeper than %.2f inches.",
                  nrow(out$boxes), nrow(out$overlaps), nrow(out$on_barrier),
                  sum(out$on_barrier$depth_in > graze_in), graze_in))
  out
}

# Read every drawn label out of the grid tree, as a box in inches measured from
# the panel's bottom left corner.
label_boxes <- function(p, width, height) {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = width, height = height)
  on.exit({ grDevices::dev.off(); unlink(f) }, add = TRUE)
  grid::grid.newpage()
  print(p)
  grid::grid.force()
  vp <- panel_viewport()
  if (!length(vp)) stop("No panel viewport was drawn.", call. = FALSE)
  grid::seekViewport(vp[1])
  gr <- repel_grobs()
  if (!length(gr)) return(NULL)
  do.call(rbind, lapply(gr, function(g) {
    w <- grid::convertWidth(grid::grobWidth(g), "in", TRUE)
    h <- grid::convertHeight(grid::grobHeight(g), "in", TRUE)
    x <- grid::convertX(g$x, "in", TRUE)
    y <- grid::convertY(g$y, "in", TRUE)
    hj <- if (is.null(g$hjust)) 0.5 else g$hjust
    vj <- if (is.null(g$vjust)) 0.5 else g$vjust
    data.frame(label = gsub("\n", " ", g$label),
               x1 = x - hj * w, x2 = x + (1 - hj) * w,
               y1 = y - vj * h, y2 = y + (1 - vj) * h,
               stringsAsFactors = FALSE)
  }))
}

# The label grobs ggrepel drew, with the barrier points and the halo copies
# taken out. A haloed name is drawn many times over, once in the paper color
# for each offset of the halo and once on top in its own color, so a reading
# that keeps them all finds every name meeting itself.
repel_grobs <- function(halo = MAP_PAPER) {
  gr <- grid::grid.get("repelgrob", grep = TRUE, global = TRUE)
  if (is.null(gr)) return(list())
  if (grid::is.grob(gr)) gr <- list(gr)
  halo_rgb <- grDevices::col2rgb(halo)
  Filter(function(g) {
    if (is.null(g$label) || !length(g$label) || !nzchar(g$label)) return(FALSE)
    col <- tryCatch(as.character(g$gp$col)[1], error = function(e) NA_character_)
    if (is.na(col)) return(TRUE)
    same <- tryCatch(identical(as.vector(grDevices::col2rgb(col)),
                               as.vector(halo_rgb)), error = function(e) FALSE)
    !same
  }, gr)
}
