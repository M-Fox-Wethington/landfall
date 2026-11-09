test_that("both glyphs draw, and they are not the same shape", {
  dart <- north_arrow_circle("dart")
  delta <- north_arrow_circle("delta")
  expect_true(grid::is.grob(dart))
  expect_true(grid::is.grob(delta))
  poly <- function(g) Filter(function(x) inherits(x, "polygon"), g$children)[[1]]
  expect_false(identical(length(poly(dart)$x), length(poly(delta)$x)))
})

test_that("the glyph stays inside the ring", {
  for (glyph in c("dart", "delta")) {
    g <- north_arrow_circle(glyph)
    p <- Filter(function(x) inherits(x, "polygon"), g$children)[[1]]
    x <- as.numeric(p$x); y <- as.numeric(p$y)
    r <- sqrt((x - landfall:::ARROW_CX)^2 + (y - landfall:::ARROW_CY)^2)
    expect_lt(max(r), landfall:::ARROW_R)
  }
})

test_that("the arrow is drawn in a square viewport, so the ring is round", {
  g <- north_arrow_circle("dart")
  # The unit's name is read with unitType(), because grid stores it as an
  # integer code and the attribute comes back as "6".
  expect_equal(grid::unitType(g$vp$width), "snpc")
  expect_equal(grid::unitType(g$vp$height), "snpc")
})

test_that("a halo adds a backing disc and a halo under the letter", {
  plain <- north_arrow_circle("delta")
  haloed <- north_arrow_circle("delta", halo = "#ffffff")
  # One disc, plus one copy of the letter for each offset of its halo.
  expect_equal(length(haloed$children) - length(plain$children), 17)
  discs <- Filter(function(x) inherits(x, "circle"), haloed$children)
  expect_equal(length(discs), 2)
  # The backing sits outside the ring, or the ring would be drawn over.
  expect_gt(as.numeric(discs[[1]]$r), as.numeric(discs[[2]]$r))
})

test_that("an unknown glyph is refused", {
  expect_error(north_arrow_circle("compass"))
})
