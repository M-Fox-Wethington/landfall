glyph_polygon <- function(g) Filter(function(x) inherits(x, "polygon"), g$children)[[1]]
glyph_xy <- function(name) {
  p <- glyph_polygon(north_arrow_circle(name))
  cbind(as.numeric(p$x), as.numeric(p$y))
}
GLYPHS <- c("blade", "sastrugi", "dart")

test_that("every glyph draws, and no two are the same shape", {
  for (glyph in GLYPHS) expect_true(grid::is.grob(north_arrow_circle(glyph)))
  # The blade has five corners, the dart four, and the sastrugi is sampled from
  # four cubic curves, so all three differ in their vertex count alone.
  expect_equal(nrow(glyph_xy("blade")), 5L)
  expect_equal(nrow(glyph_xy("dart")), 4L)
  expect_gt(nrow(glyph_xy("sastrugi")), 100L)
})

test_that("every glyph reaches the same radius inside the ring", {
  # Each is scaled to GLYPH_R rather than to its own height, so all three sit in
  # the ring with identical clearance. Fitting by height instead let a glyph
  # whose widest point is low grow until its tip crowded the ring.
  for (glyph in GLYPHS) {
    p <- glyph_xy(glyph)
    r <- sqrt((p[, 1] - landfall:::ARROW_CX)^2 + (p[, 2] - landfall:::ARROW_CY)^2)
    expect_equal(max(r), landfall:::GLYPH_R, tolerance = 1e-9)
    expect_lt(max(r), landfall:::ARROW_R)
  }
})

test_that("the blade keeps the proportions it was chosen at", {
  p <- glyph_xy("blade")
  x <- p[, 1]; y <- p[, 2]
  # Drawn 0.480 as wide as it is tall. The number is pinned here and not only in
  # the constant, because squaring up a shape like this is the first thing a
  # later pass reaches for.
  expect_equal(diff(range(x)) / diff(range(y)), 0.480, tolerance = 0.005)
  # Its lowest corner is also its leftmost, which is the tack of the sail and
  # the source of the asymmetry.
  expect_equal(which.min(y), which.min(x))
})

test_that("the sastrugi leans, which is the whole character of it", {
  p <- glyph_xy("sastrugi")
  x <- p[, 1]; y <- p[, 2]
  # Its crest stands well right of the middle of its own width, where a
  # symmetric glyph would put it at the half way mark.
  expect_gt((x[which.max(y)] - min(x)) / diff(range(x)), 0.55)
  # And its lowest point is away at the other side, which is the long drift.
  expect_lt((x[which.min(y)] - min(x)) / diff(range(x)), 0.05)
})

test_that("the arrow is drawn in a square viewport, so the ring is round", {
  g <- north_arrow_circle("dart")
  # The unit's name is read with unitType(), because grid stores it as an
  # integer code and the attribute comes back as "6".
  expect_equal(grid::unitType(g$vp$width), "snpc")
  expect_equal(grid::unitType(g$vp$height), "snpc")
})

test_that("a halo adds a backing disc and a halo under the letter", {
  plain <- north_arrow_circle("blade")
  haloed <- north_arrow_circle("blade", halo = "#ffffff")
  # One disc, plus one copy of the letter for each offset of its halo.
  expect_equal(length(haloed$children) - length(plain$children), 17)
  discs <- Filter(function(x) inherits(x, "circle"), haloed$children)
  expect_equal(length(discs), 2)
  # The backing sits outside the ring, or the ring would be drawn over.
  expect_gt(as.numeric(discs[[1]]$r), as.numeric(discs[[2]]$r))
})

test_that("the retired glyph is gone and an unknown one is refused", {
  # The shape this replaced was the Starfleet insignia. Asking for it by its old
  # name must fail rather than quietly draw something else.
  expect_error(north_arrow_circle("delta"))
  expect_error(north_arrow_circle("compass"))
})
