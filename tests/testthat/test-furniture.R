test_that("the panel of a square window is square, and a title takes height from it", {
  w <- site_window("NEKO", radius_km = 25)
  bare <- map_panel_inches(w, 12, 12)
  expect_length(bare, 2)
  expect_true(all(is.finite(bare)))
  expect_equal(bare[1], bare[2], tolerance = 1e-6)
  expect_true(bare[1] < 12)
  titled <- map_panel_inches(w, 12, 12, title = "Neko Harbor",
                             caption = "Relief: REMA v2.0.",
                             theme_extra = landfall:::theme_site_map_titled())
  expect_true(titled[1] < bare[1])
})

test_that("the reserved rectangles hold the furniture that was measured on a render", {
  # These four numbers are the bounding box of every pixel that changed when
  # the map was drawn twice, once with the two annotations and once without,
  # at a panel of 10.9426 inches. They are inches from the panel's bottom left
  # corner for the bar, and from its top right corner for the arrow.
  pw <- 10.9426
  b <- furniture_boxes(c(pw, pw))
  bar <- b[b$what == "scale bar", ]
  arrow <- b[b$what == "north arrow", ]

  expect_true(bar$x1 < 0.309 && bar$x2 > 2.949)
  expect_true(bar$y1 < 0.308 && bar$y2 > 0.421)
  # The arrow's ink ran 0.187 to 0.280 inches in from the panel's right edge
  # and 0.088 to 0.501 inches down from its top.
  expect_true(arrow$x1 < pw - 0.280 && arrow$x2 > pw - 0.187)
  expect_true(arrow$y1 < pw - 0.501 && arrow$y2 > pw - 0.088)
})

test_that("the bar's reservation grows with the panel and the arrow's does not", {
  small <- furniture_boxes(c(6.4426, 6.4426))
  big <- furniture_boxes(c(10.9426, 10.9426))
  bar <- function(d) d[d$what == "scale bar", ]
  arrow <- function(d) d[d$what == "north arrow", ]
  # The bar is drawn at a round length no longer than a share of the panel, so
  # its reservation is that share. The arrow is a fixed size in centimeters.
  expect_true(bar(big)$x2 > bar(small)$x2)
  expect_equal(bar(big)$x1, bar(small)$x1)
  expect_equal(arrow(big)$x2 - arrow(big)$x1, arrow(small)$x2 - arrow(small)$x1)
  expect_equal(arrow(big)$x2 - 10.9426, arrow(small)$x2 - 6.4426)
})

test_that("the barrier lattice covers its rectangles and lands in the window", {
  w <- site_window("NEKO", radius_km = 25)
  panel <- c(10, 10)
  boxes <- furniture_boxes(panel)
  pts <- landfall:::lattice_in_window(boxes, w, panel, step_in = 0.08)
  expect_s3_class(pts, "sf")
  expect_equal(sf::st_crs(pts), sf::st_crs(w$crs))
  xy <- sf::st_coordinates(pts)
  expect_true(all(xy[, 1] >= w$xlim[1] & xy[, 1] <= w$xlim[2]))
  expect_true(all(xy[, 2] >= w$ylim[1] & xy[, 2] <= w$ylim[2]))

  # A name is pushed only by a point inside its own box, so no gap in the
  # lattice may be wider than the step it was asked for.
  in_x <- (xy[, 1] - w$xlim[1]) / (diff(w$xlim) / panel[1])
  in_y <- (xy[, 2] - w$ylim[1]) / (diff(w$ylim) / panel[2])
  bar <- boxes[boxes$what == "scale bar", ]
  k <- in_x <= bar$x2 + 1e-6 & in_y <= bar$y2 + 1e-6
  expect_true(max(diff(sort(unique(round(in_x[k], 6))))) <= 0.08 + 1e-6)
  expect_true(max(diff(sort(unique(round(in_y[k], 6))))) <= 0.08 + 1e-6)
})

test_that("a marker barrier fills its disc rather than ringing it", {
  w <- site_window("NEKO", radius_km = 25)
  panel <- c(10, 10)
  ctr <- sf::st_as_sf(data.frame(x = mean(w$xlim), y = mean(w$ylim)),
                      coords = c("x", "y"), crs = w$crs)
  r_in <- landfall:::marker_radius_in(5.6)
  pts <- landfall:::marker_barrier(ctr, r_in, w, panel, step_in = 0.02)
  xy <- sf::st_coordinates(pts)
  m_per_in <- diff(w$xlim) / panel[1]
  d_in <- sqrt((xy[, 1] - mean(w$xlim))^2 + (xy[, 2] - mean(w$ylim))^2) / m_per_in
  expect_true(max(d_in) <= r_in + 1e-9)
  # A ring would leave the middle empty, and a label well inside a ring
  # touches nothing, so there has to be a point at the center.
  expect_true(min(d_in) < 1e-9)
  expect_true(max(diff(sort(unique(round(d_in, 6))))) <= 0.02 + 1e-6)
})

test_that("a marker's radius is half its drawn diameter", {
  expect_equal(landfall:::marker_radius_in(5.6), 5.6 * ggplot2::.pt / 2 / 72)
  expect_true(landfall:::marker_radius_in(2.6) < landfall:::marker_radius_in(5.6))
})

test_that("furniture_boxes refuses a panel it cannot measure from", {
  expect_error(furniture_boxes(c(NA, 10)))
  expect_error(furniture_boxes(10))
})

test_that("the label measurement reads back the labels that were drawn", {
  w <- site_window("NEKO", radius_km = 25)
  d <- sf::st_as_sf(
    data.frame(x = mean(w$xlim) + c(-12000, 9000, 0),
               y = mean(w$ylim) + c(8000, -6000, 0),
               label = c("Alpha", "Bravo", "Charlie")),
    coords = c("x", "y"), crs = w$crs)
  p <- ggplot2::ggplot() +
    landfall:::name_layer(
      sf::st_sf(label = d$label, size = 9, face = "plain",
                color = landfall:::MAP_TEXT, geometry = sf::st_geometry(d)),
      NULL) +
    ggplot2::coord_sf(crs = w$crs, xlim = w$xlim, ylim = w$ylim, expand = FALSE)
  out <- check_map_labels(p, 8, 8)
  expect_type(out, "list")
  expect_setequal(out$boxes$label, c("Alpha", "Bravo", "Charlie"))
  # The halo is drawn many times over in the paper color, so a reading that
  # kept those copies would find every name meeting itself.
  expect_equal(nrow(out$boxes), 3)
  expect_equal(nrow(out$overlaps), 0)
  expect_true(all(out$boxes$x2 > out$boxes$x1))
  expect_true(all(out$boxes$y2 > out$boxes$y1))
})
