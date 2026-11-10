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
#
# The same arrow is in the frazil package, which draws the sea ice and region
# sheets, and in templates/r/bri_figure_style.R in the design-library
# repository. The geometry below is meant to be identical in all three, because
# three families of BRI map drawing three slightly different arrows is the kind
# of drift nobody notices until the sheets sit side by side. What differs,
# deliberately, is the defaults: these are plain black and white, because here
# the caller sets them.
#
# Every variant tried on the way to this shape is kept in the design-library
# repository under cartography/symbols/north-arrows, as SVG with its own
# measurements, including the glyph this one replaced.

# The ring and the letter, as fractions of the square viewport.
ARROW_CX <- 0.50
ARROW_CY <- 0.40
ARROW_R  <- 0.37
ARROW_N_Y <- 0.93

# How far the furthest point of a glyph sits from the ring's center. Every glyph
# is scaled to this rather than to its own height, so each sits inside the ring
# with the same clearance whatever its shape, and a glyph whose widest point is
# low cannot grow until its tip bursts the ring.
GLYPH_R <- 0.3306

# A cubic curve, sampled, so a curved edge can be drawn as part of a polygon.
cubic_curve <- function(p0, p1, p2, p3, n = 45) {
  t <- seq(0, 1, length.out = n)
  cbind((1 - t)^3 * p0[1] + 3 * (1 - t)^2 * t * p1[1] +
          3 * (1 - t) * t^2 * p2[1] + t^3 * p3[1],
        (1 - t)^3 * p0[2] + 3 * (1 - t)^2 * t * p1[2] +
          3 * (1 - t) * t^2 * p2[2] + t^3 * p3[2])
}

# The blade: a sail, with a hollow luff on the left, a broken leech on the right
# and the tack at the bottom left, in its own unit box. Five corners joined by
# straight edges, which is what gives it the angular reading; the curved version
# it came from is in the symbol set under the name `sail`.
blade_outline <- function() {
  cbind(c(0.420, 0.265, 0.000, 1.000, 0.790),
        c(1.000, 0.470, 0.000, 0.290, 0.620))
}

# The dart: a sharp tip, two feet and a notch between them.
dart_outline <- function() {
  cbind(c(0.50, 0.88, 0.50, 0.12), c(1.00, 0.02, 0.34, 0.02))
}

# The sastrugi: a wind-carved snow ridge, with a sharp crest set toward the
# wind, a short scoured face on the windward side and a long shallow drift
# falling away to leeward. Four corners joined by cubic curves. Unlike the
# blade its character is in the curvature and not in the corners, so the
# control points are given outright instead of as corners alone.
sastrugi_outline <- function() {
  edges <- list(
    list(c(0.61000000, 1.00000000), c(0.44440736, 0.59421732),
         c(0.04481180, 0.32251147), c(0.00000000, 0.00000000)),
    list(c(0.00000000, 0.00000000), c(0.18142667, 0.09373878),
         c(0.52852068, 0.21111103), c(0.69000000, 0.18000000)),
    list(c(0.69000000, 0.18000000), c(0.75837052, 0.19455294),
         c(0.85764477, 0.15713587), c(0.93000000, 0.08500000)),
    list(c(0.93000000, 0.08500000), c(0.85337438, 0.34803542),
         c(0.73522861, 0.69126680), c(0.61000000, 1.00000000)))
  do.call(rbind, lapply(edges, function(e) cubic_curve(e[[1]], e[[2]], e[[3]], e[[4]])))
}

# How wide each glyph is drawn for a given height. Both are a good deal taller
# than they are wide, which is the first thing that goes wrong when a shape like
# this is redrawn from memory.
BLADE_ASPECT    <- 0.480
SASTRUGI_ASPECT <- 0.700
DART_ASPECT     <- 0.780

# Fit an outline into the ring at GLYPH_R, drawn at the given width over height.
fit_in_ring <- function(xy, aspect) {
  w <- diff(range(xy[, 1])); h <- diff(range(xy[, 2]))
  s <- cbind((xy[, 1] - mean(range(xy[, 1]))) / h * aspect / (w / h),
             (xy[, 2] - mean(range(xy[, 2]))) / h)
  k <- GLYPH_R / max(sqrt(s[, 1]^2 + s[, 2]^2))
  cbind(ARROW_CX + s[, 1] * k, ARROW_CY + s[, 2] * k)
}

#' A circled north arrow
#'
#' A glyph inside a ring with the letter N above it, in the style of the BRI
#' wall sheets. Pass the result to `ggspatial::annotation_north_arrow()` as its
#' `style`, or give it to [site_map_plot()] by name.
#'
#' `glyph = "blade"` is the house glyph, a five-cornered sail with a hollow luff
#' and a broken leech, drawn with straight edges. `glyph = "sastrugi"` is the
#' curved alternative, a wind-carved snow ridge with a sharp crest, a short
#' scoured windward face and a long drift to leeward. `glyph = "dart"` is a
#' plain arrowhead, for a sheet where either of those reads as a logo. `filled = TRUE` fills
#' the ring and draws the glyph in `bg` instead, which reads at a smaller size
#' and against a busy map.
#'
#' The whole arrow is drawn inside a square viewport centered in the box it is
#' given, so the ring is round at any width and height.
#'
#' @param glyph `"blade"`, `"sastrugi"` or `"dart"`.
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
#' grid::grid.draw(north_arrow_circle("blade"))
#' @export
north_arrow_circle <- function(glyph = c("blade", "sastrugi", "dart"), filled = FALSE,
                               halo = NA, line_width = 1, line_col = "black",
                               fill = "black", bg = "white",
                               text_col = "black", text_family = "",
                               text_face = NULL, text_size = 10) {
  glyph <- match.arg(glyph)
  stopifnot(length(filled) == 1, is.logical(filled), !is.na(filled))
  p <- switch(glyph,
              blade    = fit_in_ring(blade_outline(), BLADE_ASPECT),
              sastrugi = fit_in_ring(sastrugi_outline(), SASTRUGI_ASPECT),
              dart     = fit_in_ring(dart_outline(), DART_ASPECT))
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
