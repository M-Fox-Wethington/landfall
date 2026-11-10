# --- North arrows --------------------------------------------------------------
#
# ggspatial's own styles are a bare line and triangle. These are the circled
# form the BRI sheets use: a glyph inside a ring, with the N above it. A style
# is a function of no required arguments returning a grid object, which is the
# contract annotation_north_arrow() expects, so these drop straight into it.
#
# Everything is drawn inside a square viewport centered in the box the
# annotation is given, so the ring stays round and the glyph keeps its
# proportions whatever width and height it is drawn at.

# The ring and the glyph inside it, as fractions of the square viewport.
ARROW_CX <- 0.50
ARROW_CY <- 0.40
ARROW_R  <- 0.37
ARROW_N_Y <- 0.93

# A cubic curve, sampled, so an edge can be drawn as a polygon.
cubic_curve <- function(p0, p1, p2, p3, n = 45) {
  t <- seq(0, 1, length.out = n)
  cbind((1 - t)^3 * p0[1] + 3 * (1 - t)^2 * t * p1[1] +
          3 * (1 - t) * t^2 * p2[1] + t^3 * p3[1],
        (1 - t)^3 * p0[2] + 3 * (1 - t)^2 * t * p1[2] +
          3 * (1 - t) * t^2 * p2[2] + t^3 * p3[2])
}

# The dart: a sharp tip, two feet and a notch between them, in its own unit box.
dart_outline <- function() {
  cbind(c(0.50, 0.88, 0.50, 0.12), c(1.00, 0.02, 0.34, 0.02))
}

# How wide the delta is drawn for a given height. It is a good deal taller
# than it is wide, which is the first thing that goes wrong when the shape is
# drawn from memory.
DELTA_ASPECT <- 0.626

# The delta: an apex at the top, a long left edge sweeping out to a tip at the
# very bottom, a concave base that climbs to a high point right of centre, and
# a shorter right edge running back up to the apex. It is not symmetric, and
# the asymmetry is the whole character of it. The four corners and the curves
# between them were fitted to a shape measured off a drawing of it, within
# about one part in a hundred of the box, which is a few thousandths of an
# inch at the size the arrow is drawn.
delta_outline <- function() {
  apex  <- c(0.503, 1.000)
  left  <- c(0.006, 0.000)
  peak  <- c(0.676, 0.368)
  right <- c(1.000, 0.095)
  rbind(
    cubic_curve(apex,  c(0.200, 0.690), c(0.022, 0.310), left),
    cubic_curve(left,  c(0.170, 0.145), c(0.500, 0.358), peak),
    cubic_curve(peak,  c(0.790, 0.355), c(0.925, 0.245), right),
    cubic_curve(right, c(0.950, 0.400), c(0.775, 0.745), apex))
}

# Fit an outline into the ring, keeping its proportions.
fit_in_ring <- function(xy, height, aspect) {
  cbind(ARROW_CX + (xy[, 1] - 0.5) * height * aspect,
        ARROW_CY + (xy[, 2] - 0.5) * height)
}

#' A circled north arrow
#'
#' A glyph inside a ring with the letter N above it, in the style of the BRI
#' wall sheets. Pass the result to `ggspatial::annotation_north_arrow()` as its
#' `style`, or give it to [site_map_plot()] by name.
#'
#' `glyph = "dart"` is a plain arrowhead. `glyph = "delta"` is the swept,
#' asymmetric wedge, drawn solid and without the star that sometimes sits
#' inside it, which would be mush at this size. `filled = TRUE` fills the
#' ring and draws the glyph in
#' `bg` instead, which reads at a smaller size and against a busy map.
#'
#' The whole arrow is drawn inside a square viewport centered in the box it is
#' given, so the ring is round at any width and height.
#'
#' @param glyph `"dart"` or `"delta"`.
#' @param filled Fill the ring and draw the glyph in `bg`.
#' @param halo Color of the backing drawn behind the ring and the letter, or
#'   `NA` for none. On a map the arrow lands on whatever is under it, and
#'   without a backing the ring and the N are read against the terrain.
#' @param line_width Width of the ring, as a multiplier.
#' @param line_col Color of the ring.
#' @param fill Color of the glyph, and of the ring when `filled` is `TRUE`.
#' @param bg Color the glyph is drawn in when `filled` is `TRUE`.
#' @param text_col,text_family,text_face,text_size The letter N.
#' @return A `grid` object.
#' @examples
#' grid::grid.newpage()
#' grid::grid.draw(north_arrow_circle("delta"))
#' @export
north_arrow_circle <- function(glyph = c("dart", "delta"), filled = FALSE,
                               halo = NA, line_width = 1, line_col = "black",
                               fill = "black", bg = "white",
                               text_col = "black", text_family = "",
                               text_face = NULL, text_size = 10) {
  glyph <- match.arg(glyph)
  stopifnot(length(filled) == 1, is.logical(filled), !is.na(filled))
  xy <- if (glyph == "delta") delta_outline() else dart_outline()
  # The delta is wider and shorter than the dart, so each is sized to sit
  # inside the ring with the same clearance.
  p <- if (glyph == "delta") {
    fit_in_ring(xy, 1.52 * ARROW_R, DELTA_ASPECT)
  } else {
    fit_in_ring(xy, 1.55 * ARROW_R, 0.78)
  }
  letter <- function(x, y, col) {
    grid::textGrob("N", x = x, y = y, default.units = "npc",
                   gp = grid::gpar(col = col, fontsize = text_size,
                                   fontfamily = text_family, fontface = text_face))
  }
  parts <- list()
  if (!is.na(halo)) {
    # A disc under the ring, and the letter set many times over around its own
    # position, which is how the names on these maps are haloed.
    parts <- c(parts, list(grid::circleGrob(
      x = ARROW_CX, y = ARROW_CY, r = ARROW_R + 0.03, default.units = "npc",
      gp = grid::gpar(col = NA, fill = halo))))
    th <- seq(0, 2 * pi, length.out = 17)[-17]
    parts <- c(parts, lapply(th, function(a) {
      letter(ARROW_CX + 0.014 * cos(a), ARROW_N_Y + 0.014 * sin(a), halo)
    }))
  }
  parts <- c(parts, list(
    grid::circleGrob(
      x = ARROW_CX, y = ARROW_CY, r = ARROW_R, default.units = "npc",
      gp = grid::gpar(col = line_col, lwd = line_width * 1.4,
                      fill = if (filled) fill else NA)),
    grid::polygonGrob(
      x = p[, 1], y = p[, 2], default.units = "npc",
      gp = grid::gpar(col = NA, fill = if (filled) bg else fill)),
    letter(ARROW_CX, ARROW_N_Y, text_col)))
  grid::gTree(
    children = do.call(grid::gList, parts),
    # A square viewport, so the ring is a circle and not an ellipse when the
    # annotation's box is not square.
    vp = grid::viewport(width = grid::unit(1, "snpc"),
                        height = grid::unit(1, "snpc")))
}
