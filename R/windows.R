# --- Map windows --------------------------------------------------------------

# A polar stereographic CRS turned to `lon_0`, true at `lat_ts`.
stere_crs <- function(lon_0, lat_ts = -71) {
  sf::st_crs(sprintf(paste("+proj=stere +lat_0=-90 +lat_ts=%g +lon_0=%g",
                           "+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"), lat_ts, lon_0))
}

#' The map window around a site
#'
#' A square frame of `radius_km` on each side of the site, in a polar
#' stereographic projection turned to the site's own meridian, so true north
#' is up at the site. The BRI Peninsula maps use a projection turned to 60W
#' instead; pass `lon_0 = -60` to match them.
#'
#' @param site A site, as [as_site()] accepts.
#' @param radius_km Half-width of the frame in kilometers.
#' @param lon_0 Central meridian of the projection. Defaults to the site's
#'   longitude.
#' @param lat_ts Latitude of true scale.
#' @param pad_m How far beyond the frame the layers are read, in meters.
#' @return An object of class `landfall_window`: a list with the `site`,
#'   `radius_km`, `crs`, the site's `center` in projected meters, `xlim`,
#'   `ylim`, `bbox`, the padded `bbox_3031` in EPSG:3031 and its WKT.
#' @export
site_window <- function(site, radius_km = 25, lon_0 = NULL, lat_ts = -71, pad_m = 5000) {
  site <- as_site(site)
  if (is.null(lon_0)) lon_0 <- site$lon
  crs <- stere_crs(lon_0, lat_ts)
  ctr <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_point(c(site$lon, site$lat)), crs = 4326), crs))
  r <- radius_km * 1000
  xlim <- ctr[1] + c(-r, r)
  ylim <- ctr[2] + c(-r, r)
  bbox <- sf::st_bbox(c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2]),
                      crs = crs)
  padded <- sf::st_buffer(sf::st_as_sfc(bbox), pad_m)
  in_3031 <- sf::st_transform(padded, 3031)
  structure(list(site = site, radius_km = radius_km, lon_0 = lon_0, crs = crs,
                 center = c(x = ctr[1], y = ctr[2]), xlim = xlim, ylim = ylim,
                 bbox = bbox, pad_m = pad_m, padded = padded,
                 bbox_3031 = sf::st_bbox(in_3031),
                 wkt_3031 = sf::st_as_text(sf::st_geometry(in_3031))),
            class = "landfall_window")
}

#' @export
print.landfall_window <- function(x, ...) {
  cat(sprintf("Window of %g km around %s (%s), projection turned to %g\n",
              x$radius_km, x$site$name, x$site$code, x$lon_0))
  invisible(x)
}

# Cache name stem for a window: neko_r25_lon-62.5.
window_stem <- function(window) {
  sprintf("%s_r%g_lon%g", tolower(window$site$code), window$radius_km,
          round(window$lon_0, 1))
}
