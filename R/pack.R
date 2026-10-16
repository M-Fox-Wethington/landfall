# --- Packing for offline use ---------------------------------------------------------

#' What a list of sites needs, and whether it is on disk
#'
#' @param sites A vector of MAPPPD codes, or a list of sites as [as_site()]
#'   accepts.
#' @param radius_km Half-width of the maps in kilometers.
#' @param paths The list from [landfall_paths()].
#' @return A data frame with one row per site and REMA tile: `code`, `name`,
#'   `tile`, `on_disk`, plus the attribute `layers`, a named logical vector
#'   saying which ADD layers are on disk.
#' @examples
#' \dontrun{
#' check_offline(c("NEKO", "CUVE", "DANC"), radius_km = 25)
#' }
#' @export
check_offline <- function(sites, radius_km = 25, paths = landfall_paths()) {
  sites <- lapply(sites, as_site)
  rows <- lapply(sites, function(s) {
    w <- site_window(s, radius_km)
    t <- rema_tiles_for_bbox(w$bbox_3031)
    data.frame(code = s$code, name = s$name, tile = t$name,
               on_disk = file.exists(file.path(paths$rema_dir, t$file)),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  layers <- c(coastline_hr = has_path(paths, "coastline_hr"),
              coastline = has_path(paths, "coastline"),
              iceshelves = has_path(paths, "iceshelves"),
              contours = has_path(paths, "contours"), rock = has_path(paths, "rock"),
              rema_1km = has_path(paths, "rema_1km"))
  structure(out, layers = layers)
}

#' Pack everything a list of sites needs for offline use
#'
#' Fetches the REMA 32 m tiles that the sites' maps need and are not yet in
#' `rema_dir`, checks that the coastline and the other ADD layers are on
#' disk, and builds each site's relief so that nothing is computed on the
#' trip. Run it with a connection before leaving.
#'
#' @inheritParams check_offline
#' @param build_terrain Build and cache the relief for each site as well.
#' @param quiet Suppress progress messages.
#' @return The table from [check_offline()] after packing, invisibly.
#' @examples
#' \dontrun{
#' pack_sites(c("NEKO", "CUVE", "DANC", "PETE"), radius_km = 25)
#' }
#' @export
pack_sites <- function(sites, radius_km = 25, build_terrain = TRUE, quiet = FALSE,
                       paths = landfall_paths()) {
  sites <- lapply(sites, as_site)
  need <- check_offline(sites, radius_km, paths)
  layers <- attr(need, "layers")
  if (!layers[["coastline_hr"]] && !(layers[["coastline"]] && layers[["iceshelves"]])) {
    stop("No coastline on disk. Set landfall_paths(coastline_hr = ) to the ADD high-resolution ",
         "coastline GeoPackage, or coastline and iceshelves to the medium-resolution layers.",
         call. = FALSE)
  }
  missing <- unique(need$tile[!need$on_disk])
  if (length(missing)) {
    if (!quiet) message(length(missing), " REMA tile(s) to fetch: ", paste(missing, collapse = ", "))
    tiles <- do.call(rbind, lapply(sites, function(s)
      rema_tiles_for_bbox(site_window(s, radius_km)$bbox_3031)))
    tiles <- tiles[!duplicated(tiles$name) & tiles$name %in% missing, ]
    rema_fetch_tiles(tiles, paths$rema_dir, quiet = quiet)
  } else if (!quiet) {
    message("All ", length(unique(need$tile)), " REMA tile(s) are on disk.")
  }
  if (build_terrain) {
    for (s in sites) {
      w <- site_window(s, radius_km)
      build_site_terrain(w, load_coast(w, paths), paths = paths)
    }
  }
  invisible(check_offline(sites, radius_km, paths))
}
