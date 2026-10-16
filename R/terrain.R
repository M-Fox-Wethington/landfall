# --- Relief -------------------------------------------------------------------
# The BRI neighborhood-map relief: REMA elevation under a four-direction Imhof
# hillshade, colored by an eight-step hypsometric ice ramp, with the ocean
# painted wherever there is no elevation.

#' Build the relief for a window
#'
#' Mosaics the REMA 32 m tiles over the window (or crops the 1 km mosaic when
#' a tile is missing), projects the elevation onto the window's grid, masks it
#' to the coastline, shades it and colors it, and caches the result in the
#' terrain cache so a second call for the same window loads it from disk.
#'
#' @details
#' The recipe is the one of the BRI neighborhood maps. Elevation is multiplied
#' by `z_factor` and shaded from the northwest (0.45), north (0.20), northeast
#' (0.15) and west (0.20). The ice ramp runs from near white at sea level to a
#' gray-blue above 2400 m in eight steps, shadows stop at `shadow_floor`, and
#' cells with no elevation, which are water, take the ocean color so the relief
#' blends into the ocean panel.
#'
#' @param window A [site_window()].
#' @param coast The list from [load_coast()], used to mask the elevation.
#' @param grid_res Cell size of the relief in meters. `NULL` uses 50 m on the
#'   32 m tiles and 200 m on the 1 km mosaic.
#' @param z_factor Vertical exaggeration.
#' @param shadow_floor Darkest shading, as a share of the full color.
#' @param paths The list from [landfall_paths()].
#' @return A list with `rgb`, a three-band SpatRaster, `source` (`"32 m"` or
#'   `"1 km"`) and `grid_res`.
#' @export
build_site_terrain <- function(window, coast, grid_res = NULL, z_factor = 4,
                               shadow_floor = 0.55, paths = landfall_paths()) {
  dir.create(paths$terrain_cache, recursive = TRUE, showWarnings = FALSE)
  stem <- file.path(paths$terrain_cache, paste0(window_stem(window), "_relief"))
  rgb_path <- paste0(stem, "_rgb.tif")
  meta_path <- paste0(stem, "_source.txt")
  if (file.exists(rgb_path) && file.exists(meta_path)) {
    meta <- readLines(meta_path, warn = FALSE)
    message("Loading cached relief: ", basename(rgb_path), " (", meta[1], ")")
    return(list(rgb = terra::rast(rgb_path), source = meta[1],
                grid_res = as.numeric(meta[2])))
  }

  elev <- rema_dem_for(window$bbox_3031, paths)
  if (is.null(grid_res)) grid_res <- if (elev$source == "32 m") 50 else 200
  message(sprintf("Building relief at %g m from REMA %s ...", grid_res, elev$source))
  template <- terra::rast(xmin = window$xlim[1] - window$pad_m, xmax = window$xlim[2] + window$pad_m,
                          ymin = window$ylim[1] - window$pad_m, ymax = window$ylim[2] + window$pad_m,
                          resolution = grid_res, crs = window$crs$wkt)
  dem <- terra::project(elev$dem, template, method = "bilinear")
  # Mask to the coastline so REMA values over water, including frozen sea and
  # near-zero coastal artifacts, become water and not the palest ice.
  land <- c(sf::st_geometry(coast$land), sf::st_geometry(coast$shelves))
  if (length(land)) dem <- terra::mask(dem, terra::vect(sf::st_union(land)))
  dem <- terra::classify(dem, cbind(-Inf, 0, NA), right = FALSE)

  dem_x <- dem * z_factor
  slp <- terra::terrain(dem_x, "slope", unit = "radians")
  asp <- terra::terrain(dem_x, "aspect", unit = "radians")
  hs <- 0.45 * terra::shade(slp, asp, angle = 45, direction = 315) +
        0.20 * terra::shade(slp, asp, angle = 50, direction = 0) +
        0.15 * terra::shade(slp, asp, angle = 40, direction = 45) +
        0.20 * terra::shade(slp, asp, angle = 35, direction = 270)
  rm(dem_x, slp, asp)

  v_dem <- terra::values(dem, mat = FALSE)
  idx   <- findInterval(v_dem, HYPSO_BREAKS, all.inside = TRUE)
  pal   <- grDevices::col2rgb(HYPSO_PALETTE) / 255
  v_hs  <- terra::values(hs, mat = FALSE)
  v_hs[is.na(v_hs)] <- 0.5
  shade <- shadow_floor + (1 - shadow_floor) * pmin(1, pmax(0, v_hs))
  ocean <- grDevices::col2rgb(MAP_OCEAN) / 255
  na_mask <- is.na(v_dem)
  bands <- lapply(1:3, function(ch) {
    v <- pmin(1, pal[ch, idx] * shade)
    v[na_mask] <- ocean[ch, 1]
    terra::setValues(terra::rast(dem), round(v * 255))
  })
  rgb <- terra::rast(bands)
  names(rgb) <- c("red", "green", "blue")
  rgb <- terra::writeRaster(rgb, rgb_path, overwrite = TRUE, datatype = "INT1U",
                            gdal = c("COMPRESS=LZW", "TILED=YES"))
  writeLines(c(elev$source, as.character(grid_res)), meta_path)
  list(rgb = rgb, source = elev$source, grid_res = grid_res)
}
