# --- Relief -------------------------------------------------------------------
# The BRI neighborhood-map relief: REMA elevation under a four-direction Imhof
# hillshade, colored by an eight-step hypsometric ice ramp, with the ocean
# painted where the coastline says there is water and a flat land tint where
# the coastline says there is land but REMA holds no height for it. Those are
# three different things and the map draws them as three different things.
#
# REMA v2.0 measures from the WGS84 ellipsoid, which around the Antarctic
# Peninsula stands 15 to 23 m below the sea surface, so raw heights put every
# beach that much too high and a cut at zero never fires at the shore. The sea
# surface is therefore measured from the window's own water, cell by cell
# outside the drawn coastline, and taken off the model before anything is
# colored.

# Bump this when the recipe changes, so every cache built by an older one is
# rebuilt rather than reused.
TERRAIN_RECIPE <- "2"

# Land below the corrected sea surface still sits at the bottom of the ramp
# instead of being dropped, because a cell the coastline calls land is land
# whatever REMA reads there.
TERRAIN_FLOOR_M <- 0.25

# The sea surface in this window, as REMA reads it: the median height of the
# cells the coastline says are water. NA when the window holds too little
# water to measure one, and the caller then leaves the heights alone.
measure_sea_level <- function(dem, land, min_cells = 10000) {
  if (!length(land)) return(NA_real_)
  water <- terra::mask(dem, terra::vect(sf::st_as_sf(land)), inverse = TRUE)
  v <- terra::values(water, mat = FALSE)
  v <- v[!is.na(v)]
  if (length(v) < min_cells) return(NA_real_)
  stats::median(v)
}

# A file's identity for the cache signature: its name, its size and when it was
# last written. Keying on the path alone would reuse an image after the file at
# that path had been replaced.
file_identity <- function(files) {
  files <- files[!is.na(files) & nzchar(files)]
  files <- files[file.exists(files)]
  if (!length(files)) return("none")
  info <- file.info(files)
  paste(sprintf("%s|%s|%s", basename(files), info$size,
                format(info$mtime, "%Y%m%dT%H%M%S")), collapse = ";")
}

# Everything the relief is built from, as lines written beside the cached
# raster. A file name cannot carry the window's own coordinates, the grid, the
# shading, the ramp or the versions of the layers the model is masked to, so
# the cache is reused only when all of this matches.
terrain_signature <- function(window, coast, grid_res, z_factor, shadow_floor,
                              collar_m, elev_source, elev_files) {
  c(paste0("recipe=", TERRAIN_RECIPE),
    paste0("code=", window$site$code),
    sprintf("lon=%.6f", window$site$lon),
    sprintf("lat=%.6f", window$site$lat),
    paste0("radius_km=", window$radius_km),
    sprintf("lon_0=%.6f", window$lon_0),
    sprintf("lat_ts=%.6f", window$lat_ts),
    paste0("pad_m=", window$pad_m),
    paste0("grid_res=", grid_res),
    paste0("z_factor=", z_factor),
    paste0("shadow_floor=", shadow_floor),
    paste0("collar_m=", collar_m),
    paste0("floor_m=", TERRAIN_FLOOR_M),
    paste0("elev_source=", elev_source),
    paste0("elev_files=", file_identity(elev_files)),
    paste0("coast_source=", coast$source),
    paste0("coast_files=", file_identity(coast$files)),
    paste0("palette=", paste(HYPSO_PALETTE, collapse = ",")),
    paste0("breaks=", paste(HYPSO_BREAKS, collapse = ",")),
    paste0("ocean=", MAP_OCEAN),
    paste0("bare_land=", MAP_LAND_BARE))
}

#' Build the relief for a window
#'
#' Mosaics the REMA 32 m tiles over the window (or crops the 1 km mosaic when a
#' tile is missing), projects the elevation onto the window's grid, measures the
#' sea surface from the window's own water and takes it off, shades the model,
#' colors it, and caches the result so a second call for the same window loads
#' it from disk.
#'
#' @details
#' The recipe is the one of the BRI neighborhood maps. Elevation is multiplied
#' by `z_factor` and shaded from the northwest (0.45), north (0.20), northeast
#' (0.15) and west (0.20). The ice ramp runs from near white at sea level to a
#' gray-blue above 2400 m in eight steps and shadows stop at `shadow_floor`.
#'
#' The hillshade is computed on the model cut to a collar beyond the drawn land,
#' so slopes at the shore still have neighbors, and the coloring then uses the
#' coastline itself. Water takes the ocean color. Land for which REMA holds no
#' height takes a flat land tint and the share of it is reported, because
#' painting it as ocean would invent open water where the coastline says there
#' is ground.
#'
#' The cached raster is reused only when a signature of the window, the source
#' files, the grid and every shading and color setting matches. Pass
#' `rebuild = TRUE` to force a rebuild, or see [clear_terrain_cache()].
#'
#' @param window A [site_window()].
#' @param coast The list from [load_coast()], used to measure the sea surface
#'   and to separate land from water.
#' @param grid_res Cell size of the relief in meters. `NULL` uses 50 m on the
#'   32 m tiles and 200 m on the 1 km mosaic.
#' @param z_factor Vertical exaggeration.
#' @param shadow_floor Darkest shading, as a share of the full color.
#' @param collar_m How far beyond the drawn land the model is kept for the
#'   hillshade, in meters.
#' @param rebuild Build again even when a matching cache is on disk.
#' @param paths The list from [landfall_paths()].
#' @return A list with `rgb`, a three-band SpatRaster, `source` (`"32 m"` or
#'   `"1 km"`), `grid_res` and `sea_level`, the separation taken off the model
#'   in meters.
#' @export
build_site_terrain <- function(window, coast, grid_res = NULL, z_factor = 4,
                               shadow_floor = 0.55, collar_m = 60, rebuild = FALSE,
                               paths = landfall_paths()) {
  dir.create(paths$terrain_cache, recursive = TRUE, showWarnings = FALSE)
  stem <- file.path(paths$terrain_cache, paste0(window_stem(window), "_relief"))
  rgb_path  <- paste0(stem, "_rgb.tif")
  sig_path  <- paste0(stem, "_signature.txt")
  meta_path <- paste0(stem, "_source.txt")

  # Which elevation this relief would be built from, decided from what is on
  # disk and before any raster is opened, so the signature can be compared
  # first and a matching cache costs nothing to reuse.
  tiles <- rema_tiles_for_bbox(window$bbox_3031)
  tile_files <- file.path(paths$rema_dir, tiles$file)
  have <- file.exists(tile_files)
  elev_source <- if (all(have)) "32 m" else "1 km"
  if (is.null(grid_res)) grid_res <- if (elev_source == "32 m") 50 else 200
  elev_files <- if (all(have)) tile_files else paths$rema_1km

  sig <- terrain_signature(window, coast, grid_res, z_factor, shadow_floor, collar_m,
                           elev_source, elev_files)
  if (!rebuild && file.exists(rgb_path) && file.exists(sig_path) &&
      identical(readLines(sig_path, warn = FALSE), sig)) {
    meta <- readLines(meta_path, warn = FALSE)
    message("Loading cached relief: ", basename(rgb_path), " (", meta[1], ")")
    return(list(rgb = terra::rast(rgb_path), source = meta[1],
                grid_res = as.numeric(meta[2]),
                sea_level = if (length(meta) >= 3) as.numeric(meta[3]) else NA_real_))
  }
  if (file.exists(rgb_path)) {
    message("The cached relief for this window was built from different inputs; rebuilding.")
  }

  elev <- rema_dem_for(window$bbox_3031, paths)
  message(sprintf("Building relief at %g m from REMA %s ...", grid_res, elev$source))
  template <- terra::rast(xmin = window$xlim[1] - window$pad_m,
                          xmax = window$xlim[2] + window$pad_m,
                          ymin = window$ylim[1] - window$pad_m,
                          ymax = window$ylim[2] + window$pad_m,
                          resolution = grid_res, crs = window$crs$wkt)
  dem <- terra::project(elev$dem, template, method = "bilinear")

  land_geom <- c(sf::st_geometry(coast$land), sf::st_geometry(coast$shelves))
  land <- if (length(land_geom)) sf::st_union(land_geom) else land_geom

  # Take the sea surface off the model, measured from this window's own water.
  sea_level <- measure_sea_level(dem, land)
  if (is.na(sea_level)) {
    message("Too little open water in this window to measure the sea surface, so heights ",
            "stay on the WGS84 ellipsoid and the ramp starts below the shore.")
    sea_level <- 0
  } else {
    message(sprintf("The sea surface here reads %.2f m on the ellipsoid; taking it off.",
                    sea_level))
  }
  dem <- dem - sea_level

  # Two masks answering two different questions. One says where the coastline
  # puts land, the other says where REMA holds a height. A cell can be land
  # with no height, and that is not water.
  if (length(land)) {
    land_v <- terra::vect(sf::st_as_sf(land))
    is_land <- !is.na(terra::values(terra::rasterize(land_v, dem, field = 1), mat = FALSE))
    collar <- terra::vect(sf::st_as_sf(sf::st_buffer(land, collar_m)))
    shade_dem <- terra::mask(dem, collar)
  } else {
    is_land <- rep(TRUE, terra::ncell(dem))
    shade_dem <- dem
  }

  v_dem <- terra::values(dem, mat = FALSE)
  has_dem <- !is.na(v_dem)
  v_dem[is_land & has_dem & v_dem < TERRAIN_FLOOR_M] <- TERRAIN_FLOOR_M

  dem_x <- shade_dem * z_factor
  slp <- terra::terrain(dem_x, "slope", unit = "radians")
  asp <- terra::terrain(dem_x, "aspect", unit = "radians")
  hs <- 0.45 * terra::shade(slp, asp, angle = 45, direction = 315) +
        0.20 * terra::shade(slp, asp, angle = 50, direction = 0) +
        0.15 * terra::shade(slp, asp, angle = 40, direction = 45) +
        0.20 * terra::shade(slp, asp, angle = 35, direction = 270)
  rm(dem_x, slp, asp)

  v_hs <- terra::values(hs, mat = FALSE)
  v_hs[is.na(v_hs)] <- 0.5
  shade <- shadow_floor + (1 - shadow_floor) * pmin(1, pmax(0, v_hs))

  # findInterval() alone returns 0 below the first break and the palette has
  # one color per break, so the index is clamped to the palette rather than
  # coerced inside the breaks, which would make the last color unreachable.
  idx <- pmax(1L, pmin(length(HYPSO_PALETTE), findInterval(v_dem, HYPSO_BREAKS)))
  pal   <- grDevices::col2rgb(HYPSO_PALETTE) / 255
  ocean <- grDevices::col2rgb(MAP_OCEAN) / 255
  bare  <- grDevices::col2rgb(MAP_LAND_BARE) / 255

  relief    <- is_land & has_dem
  bare_land <- is_land & !has_dem
  water     <- !is_land
  if (any(bare_land)) {
    message(sprintf(paste("REMA holds no height for %.1f%% of the land here (%s of %s",
                          "cells); it is drawn as flat land, not as water."),
                    100 * sum(bare_land) / sum(is_land), fmt_n(sum(bare_land)),
                    fmt_n(sum(is_land))))
  }

  bands <- lapply(1:3, function(ch) {
    v <- numeric(length(v_dem))
    v[relief]    <- pmin(1, pal[ch, idx[relief]] * shade[relief])
    v[bare_land] <- bare[ch, 1]
    v[water]     <- ocean[ch, 1]
    terra::setValues(terra::rast(dem), round(v * 255))
  })
  rgb <- terra::rast(bands)
  names(rgb) <- c("red", "green", "blue")
  rgb <- terra::writeRaster(rgb, rgb_path, overwrite = TRUE, datatype = "INT1U",
                            gdal = c("COMPRESS=LZW", "TILED=YES"))
  writeLines(c(elev$source, as.character(grid_res), as.character(sea_level)), meta_path)
  writeLines(sig, sig_path)
  list(rgb = rgb, source = elev$source, grid_res = grid_res, sea_level = sea_level)
}

#' Remove cached relief rasters
#'
#' The relief is cached per window and rebuilt when its inputs change, so this
#' is for clearing disk space or for forcing a rebuild by hand.
#'
#' @param site A site whose cached relief to remove, as [as_site()] accepts, or
#'   `NULL` for every cached relief.
#' @param paths The list from [landfall_paths()].
#' @return The removed paths, invisibly.
#' @examples
#' \dontrun{
#' clear_terrain_cache("NEKO")
#' }
#' @export
clear_terrain_cache <- function(site = NULL, paths = landfall_paths()) {
  dir <- paths$terrain_cache
  if (is.null(dir) || !dir.exists(dir)) {
    message("No terrain cache on disk.")
    return(invisible(character(0)))
  }
  pattern <- if (is.null(site)) "_relief_" else
    paste0("^", tolower(as_site(site)$code), "_.*_relief_")
  f <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(f)) file.remove(f)
  message("Removed ", length(f), " cached relief file(s).")
  invisible(f)
}
