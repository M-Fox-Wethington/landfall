# --- REMA 32 m tiles ------------------------------------------------------------
# The Polar Geospatial Center serves the REMA v2.0 32 m mosaic as 100 km tiles
# in EPSG:3031, named {row}_{col}. Tile column c covers x from (c - 31) * 100 km
# to (c - 30) * 100 km, and tile row r covers y from (r - 31) * 100 km to
# (r - 30) * 100 km, each with a 96 m overlap on every edge. The formula was
# checked against eight tiles on disk on 2026-10-16.
#
# The grid is arithmetic and the mosaic is not: a tile index that falls entirely
# on open sea has no archive to download. That is a gap in coverage and not a
# transfer that failed, so the two are told apart and reported differently.

REMA_TILE_M   <- 1e5
REMA_INDEX_0  <- 31
REMA_BASE_URL <- "https://data.pgc.umn.edu/elev/dem/setsm/REMA/mosaic/v2.0/32m"

#' Which REMA 32 m tiles cover a box
#'
#' The indices come from the grid formula, so a tile the mosaic does not carry
#' is listed here all the same. [rema_fetch_tiles()] asks the Polar Geospatial
#' Center which of them exist.
#'
#' @param bbox_3031 An sf bbox in EPSG:3031.
#' @return A data frame with one row per tile: `row`, `col`, `name` (such as
#'   `"43_06"`), `file` (the tile's file name) and `url`.
#' @examples
#' rema_tiles_for_bbox(sf::st_bbox(c(xmin = -2450000, ymin = 1150000,
#'                                   xmax = -2380000, ymax = 1210000),
#'                                 crs = sf::st_crs(3031)))
#' @export
rema_tiles_for_bbox <- function(bbox_3031) {
  cols <- seq(floor(bbox_3031[["xmin"]] / REMA_TILE_M), floor(bbox_3031[["xmax"]] / REMA_TILE_M)) +
    REMA_INDEX_0
  rows <- seq(floor(bbox_3031[["ymin"]] / REMA_TILE_M), floor(bbox_3031[["ymax"]] / REMA_TILE_M)) +
    REMA_INDEX_0
  g <- expand.grid(row = rows, col = cols)
  g$name <- sprintf("%02d_%02d", g$row, g$col)
  g$file <- paste0(g$name, "_32m_v2.0_dem.tif")
  g$url  <- sprintf("%s/%s/%s_32m_v2.0.tar.gz", REMA_BASE_URL, g$name, g$name)
  g[order(g$row, g$col), ]
}

# The extent a tile should have, for checks.
rema_tile_extent <- function(row, col, overlap_m = 96) {
  c(xmin = (col - REMA_INDEX_0) * REMA_TILE_M - overlap_m,
    xmax = (col - REMA_INDEX_0 + 1) * REMA_TILE_M + overlap_m,
    ymin = (row - REMA_INDEX_0) * REMA_TILE_M - overlap_m,
    ymax = (row - REMA_INDEX_0 + 1) * REMA_TILE_M + overlap_m)
}

# Whether the Polar Geospatial Center serves an archive at this address, asked
# without downloading it. TRUE, FALSE, or NA when the question could not be put.
rema_tile_available <- function(url, timeout = 30) {
  h <- curl::new_handle(nobody = TRUE, timeout = timeout, followlocation = TRUE)
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  if (is.null(res)) return(NA)
  res$status_code < 400
}

#' Fetch REMA 32 m tiles that are not yet on disk
#'
#' Asks the Polar Geospatial Center whether each missing tile exists, downloads
#' the ones that do, extracts the elevation file alone and discards the rest.
#' An archive is a few hundred megabytes to download; the elevation file it
#' yields is 15 to 40 MB on disk.
#'
#' @param tiles The table from [rema_tiles_for_bbox()].
#' @param rema_dir Folder holding the tiles.
#' @param quiet Suppress progress messages.
#' @return The table with `fetched`, a logical, and `status`, one of
#'   `"on disk"`, `"fetched"`, `"absent from the mosaic"`, `"download failed"`,
#'   `"no elevation file in the archive"` or `"could not be saved"`, invisibly.
#' @export
rema_fetch_tiles <- function(tiles, rema_dir = landfall_paths()$rema_dir, quiet = FALSE) {
  dir.create(rema_dir, recursive = TRUE, showWarnings = FALSE)
  tiles$fetched <- FALSE
  tiles$status <- "on disk"
  for (i in seq_len(nrow(tiles))) {
    dest <- file.path(rema_dir, tiles$file[i])
    if (file.exists(dest)) next
    available <- rema_tile_available(tiles$url[i])
    if (isFALSE(available)) {
      tiles$status[i] <- "absent from the mosaic"
      if (!quiet) {
        message("Tile ", tiles$name[i], " is not served: the mosaic carries no tile at ",
                "that index, which happens where the grid falls on open sea.")
      }
      next
    }
    if (!quiet) message("Fetching REMA tile ", tiles$name[i], " ...")
    tmp <- tempfile(pattern = paste0("rema_", tiles$name[i], "_"), fileext = ".tar.gz")
    on.exit(unlink(tmp), add = TRUE)
    old <- options(timeout = 3600)
    on.exit(options(old), add = TRUE)
    res <- tryCatch(curl::curl_download(tiles$url[i], tmp, quiet = TRUE),
                    error = function(e) NULL)
    if (is.null(res) || !file.exists(tmp)) {
      tiles$status[i] <- "download failed"
      warning("Could not download ", tiles$url[i], call. = FALSE)
      next
    }
    members <- untar(tmp, list = TRUE)
    dem <- members[grepl("_dem\\.tif$", members)]
    if (!length(dem)) {
      tiles$status[i] <- "no elevation file in the archive"
      warning("No elevation file in the archive for tile ", tiles$name[i], call. = FALSE)
      next
    }
    exdir <- tempfile(pattern = "rema_extract_")
    dir.create(exdir)
    untar(tmp, files = dem[1], exdir = exdir)
    # file.copy() reports failure in its return value and not by stopping, so a
    # tile marked fetched without this check may not be on disk at all.
    copied <- file.copy(file.path(exdir, dem[1]), dest, overwrite = TRUE)
    unlink(exdir, recursive = TRUE)
    unlink(tmp)
    if (!copied || !file.exists(dest)) {
      tiles$status[i] <- "could not be saved"
      warning("Could not save the elevation file for tile ", tiles$name[i], " to ", dest,
              call. = FALSE)
      next
    }
    tiles$fetched[i] <- TRUE
    tiles$status[i] <- "fetched"
    if (!quiet) message("  saved ", dest, sprintf(" (%.0f MB)", file.size(dest) / 1e6))
  }
  invisible(tiles)
}

# The elevation for a box: the 32 m tiles mosaicked and cropped, or the 1 km
# mosaic when a tile is missing. Returns list(dem, source, tiles, missing).
rema_dem_for <- function(bbox_3031, paths = landfall_paths(), require_32m = FALSE) {
  tiles <- rema_tiles_for_bbox(bbox_3031)
  files <- file.path(paths$rema_dir, tiles$file)
  have  <- file.exists(files)
  box   <- terra::ext(bbox_3031[["xmin"]], bbox_3031[["xmax"]],
                      bbox_3031[["ymin"]], bbox_3031[["ymax"]])
  if (all(have)) {
    rasters <- lapply(files, function(f) terra::crop(terra::rast(f), box, snap = "out"))
    dem <- if (length(rasters) == 1) rasters[[1]] else
      do.call(terra::mosaic, c(rasters, list(fun = "mean")))
    return(list(dem = dem, source = "32 m", tiles = tiles, missing = character(0)))
  }
  missing <- tiles$name[!have]
  if (require_32m || !has_path(paths, "rema_1km")) {
    stop("REMA 32 m tiles missing for this window: ", paste(missing, collapse = ", "),
         ". Run pack_sites() with a connection, which also reports any tile the mosaic ",
         "does not carry, or set landfall_paths(rema_1km = ) to fall back to the 1 km ",
         "mosaic.", call. = FALSE)
  }
  message("REMA 32 m tiles ", paste(missing, collapse = ", "),
          " are not on disk; using the 1 km mosaic.")
  dem <- terra::crop(terra::rast(paths$rema_1km), box, snap = "out")
  list(dem = dem, source = "1 km", tiles = tiles, missing = missing)
}
