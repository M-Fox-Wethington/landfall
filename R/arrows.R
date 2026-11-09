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

# A quadratic curve, sampled, so an edge can be drawn as a polygon.
quad_curve <- function(p0, p1, p2, n = 30) {
  t <- seq(0, 1, length.out = n)
  cbind((1 - t)^2 * p0[1] + 2 * (1 - t) * t * p1[1] + t^2 * p2[1],
        (1 - t)^2 * p0[2] + 2 * (1 - t) * t * p1[2] + t^2 * p2[2])
}

# A cubic curve. The delta's outer edge needs one, because it runs as a wedge
# down from the apex and then flares out to the foot, and a quadratic can do
# one or the other and not both.
cubic_curve <- function(p0, p1, p2, p3, n = 40) {
  t <- seq(0, 1, length.out = n)
  cbind((1 - t)^3 * p0[1] + 3 * (1 - t)^2 * t * p1[1] +
          3 * (1 - t) * t^2 * p2[1] + t^3 * p3[1],
        (1 - t)^3 * p0[2] + 3 * (1 - t)^2 * t * p1[2] +
          3 * (1 - t) * t^2 * p2[2] + t^3 * p3[2])
}

# A right half mirrored into a closed outline.
mirror_half <- function(m) {
  rbind(m, cbind(1 - m[rev(seq_len(nrow(m))), 1], m[rev(seq_len(nrow(m))), 2]))
}

# The dart: a sharp tip, two feet and a notch between them, in its own unit box.
dart_outline <- function() {
  cbind(c(0.50, 0.88, 0.50, 0.12), c(1.00, 0.02, 0.34, 0.02))
}

# The delta: a sharp apex, a wedge body, outer edges that flare to two swept
# feet, and a deep notch between them.
delta_outline <- function() {
  apex <- c(0.50, 1.00)
  foot <- c(0.96, 0.02)
  mirror_half(rbind(
    cubic_curve(apex, c(0.70, 0.78), c(0.74, 0.26), foot),
    quad_curve(foot, c(0.76, 0.08), c(0.50, 0.38))))
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
#' `glyph = "dart"` is a plain arrowhead. `glyph = "delta"` is a swept wedge
#' with two flared feet. `filled = TRUE` fills the ring and draws the glyph in
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
    fit_in_ring(xy, 1.44 * ARROW_R, 0.86)
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
