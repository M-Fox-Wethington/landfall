# --- Sites --------------------------------------------------------------------
# A site carries two identifiers and they do different work. `code` is a short
# label for titles, file names and cache names, and the user may set it to
# anything. `mapppd_id` is a validated MAPPPD site identifier and is the only
# thing a count record is ever retrieved by. A point of your own has no
# `mapppd_id` unless you say which MAPPPD site it stands for, so a name whose
# first four letters happen to match a real site cannot pick up that site's
# counts.

#' Look up a site by its MAPPPD code
#'
#' MAPPPD gives every penguin colony site a four-letter code, such as `NEKO`
#' for Neko Harbor. The codes, names, regions and coordinates come from the
#' `sites` table of the `mapppdr` package, which carries MAPPPD inside the
#' install, so the lookup works offline.
#'
#' @param code A four-letter MAPPPD site code. Case does not matter.
#' @return A `landfall_site`: a list with `code`, `mapppd_id`, `name`,
#'   `region`, `lon`, `lat` and `source`.
#' @examples
#' site_lookup("NEKO")
#' @export
site_lookup <- function(code) {
  code <- toupper(trimws(code))
  s <- mapppdr::sites
  i <- match(code, s$site_id)
  if (is.na(i)) {
    near <- s$site_id[agrepl(code, s$site_id, max.distance = 1)]
    stop(sprintf("No MAPPPD site has the code '%s'.%s", code,
                 if (length(near)) paste0(" Near matches: ", paste(near, collapse = ", "), ".")
                 else ""), call. = FALSE)
  }
  new_site(code = s$site_id[i], mapppd_id = s$site_id[i], name = s$site_name[i],
           region = s$region[i], lon = s$longitude[i], lat = s$latitude[i],
           source = "MAPPPD")
}

#' A site from a name and coordinates
#'
#' For a place that is not a MAPPPD site, or for a point of your own. The point
#' has no MAPPPD identity, so [colony_record()] returns nothing for it and no
#' count history is compared. Pass `mapppd_id` to say that the point stands for
#' a particular MAPPPD site, and that site's record is then used.
#'
#' @param name The site's name, used in titles.
#' @param lon,lat Longitude (east positive) and latitude (south negative) in
#'   degrees.
#' @param code An optional short code, used in file names and cache names.
#'   Defaults to the first four letters of the name in upper case. It is a
#'   label only and is never used to look up counts.
#' @param mapppd_id An optional MAPPPD site code this point stands for. It is
#'   checked against MAPPPD, and only then is that site's count record used.
#' @param region An optional region name for the subtitle.
#' @return A `landfall_site`.
#' @examples
#' site_point("Stonington Island", lon = -67.00, lat = -68.18)
#' @export
site_point <- function(name, lon, lat, code = NULL, mapppd_id = NULL, region = NULL) {
  if (is.null(code)) {
    code <- toupper(substr(gsub("[^A-Za-z]", "", name), 1, 4))
    if (!nzchar(code)) code <- "SITE"
  }
  stopifnot(is.numeric(lon), is.numeric(lat), length(lon) == 1, length(lat) == 1,
            lat <= -50, lat >= -90, lon >= -180, lon <= 180)
  if (!is.null(mapppd_id)) {
    mapppd_id <- toupper(trimws(mapppd_id))
    if (!mapppd_id %in% mapppdr::sites$site_id) {
      stop(sprintf("No MAPPPD site has the code '%s', so this point cannot stand for it.",
                   mapppd_id), call. = FALSE)
    }
  }
  new_site(code = code, mapppd_id = mapppd_id, name = name, region = region, lon = lon,
           lat = lat, source = "user")
}

new_site <- function(code, mapppd_id, name, region, lon, lat, source) {
  structure(list(code = code, mapppd_id = mapppd_id, name = name, region = region,
                 lon = lon, lat = lat, source = source), class = "landfall_site")
}

#' Coerce to a site
#'
#' Every function that takes a site accepts a four-letter MAPPPD code, the
#' result of [site_lookup()] or [site_point()], or a list with `name`, `lon`
#' and `lat`.
#'
#' @param x The site in any of those forms.
#' @return A `landfall_site`.
#' @examples
#' as_site("NEKO")
#' as_site(list(name = "Somewhere", lon = -60, lat = -65))
#' @export
as_site <- function(x) {
  if (inherits(x, "landfall_site")) return(x)
  if (is.character(x) && length(x) == 1) return(site_lookup(x))
  if (is.list(x) && all(c("name", "lon", "lat") %in% names(x))) {
    return(site_point(x$name, x$lon, x$lat, code = x$code, mapppd_id = x$mapppd_id,
                      region = x$region))
  }
  stop("A site is a four-letter MAPPPD code, a site_lookup() or site_point() result, ",
       "or a list with name, lon and lat.", call. = FALSE)
}

#' @export
print.landfall_site <- function(x, ...) {
  cat(sprintf("%s (%s), %s, %s\n", x$name, x$code,
              if (is.null(x$region)) "no region" else x$region, fmt_latlon(x$lon, x$lat)))
  cat(if (is.null(x$mapppd_id)) {
    "  Not a MAPPPD site, so it has no count record.\n"
  } else {
    sprintf("  MAPPPD site %s.\n", x$mapppd_id)
  })
  invisible(x)
}

# The site as an sf point in a CRS.
site_sf <- function(site, crs) {
  sf::st_sf(code = site$code, label = site$name,
            geometry = sf::st_sfc(sf::st_point(c(site$lon, site$lat)), crs = 4326)) |>
    sf::st_transform(crs)
}

#' Other MAPPPD sites within a radius
#'
#' @param site A site, as [as_site()] accepts.
#' @param radius_km Radius in kilometers.
#' @param crs The CRS of the returned points.
#' @return An sf object of points with `code`, `name`, `n_counts` (the number
#'   of MAPPPD counts at the site) and `dist_km`, sorted by distance. The site
#'   itself is left out when it is a MAPPPD site; a point of your own leaves
#'   every MAPPPD site in, including one whose code resembles its label.
#' @export
sites_within <- function(site, radius_km, crs = 4326) {
  site <- as_site(site)
  s <- mapppdr::sites
  pts <- sf::st_as_sf(s, coords = c("longitude", "latitude"), crs = 4326)
  ctr <- sf::st_sfc(sf::st_point(c(site$lon, site$lat)), crs = 4326)
  d <- as.numeric(sf::st_distance(pts, ctr)) / 1000
  keep <- d <= radius_km
  if (!is.null(site$mapppd_id)) keep <- keep & s$site_id != site$mapppd_id
  counts <- table(mapppdr::penguin_obs$site_id)
  out <- pts[keep, ]
  out$code <- s$site_id[keep]
  out$name <- s$site_name[keep]
  out$n_counts <- as.integer(counts[out$code])
  out$n_counts[is.na(out$n_counts)] <- 0L
  out$dist_km <- d[keep]
  out <- out[order(out$dist_km), c("code", "name", "n_counts", "dist_km")]
  sf::st_transform(out, crs)
}
