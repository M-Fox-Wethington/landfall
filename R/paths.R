#' Folders and files the package reads and writes
#'
#' The package keeps its settings in the R option `landfall.paths`. Call
#' `landfall_paths()` with arguments to set them for the session, and with no
#' arguments to read the resolved list. Only the cache has a default. The
#' coastline and the other SCAR Antarctic Digital Database (ADD) layers have to
#' point at copies on disk before a map can be drawn, and `output` has to be set
#' before a figure is written.
#'
#' An omitted argument leaves a setting as it is. Passing `NA` clears one, and
#' `reset = TRUE` clears them all, so a setting made earlier in the session can
#' be taken back rather than only replaced.
#'
#' @details
#' The ADD layers default to this layout under `spatial`, which is the layout
#' of the BRI priority-map bundle. Each can also be set on its own.
#' * `coastline_hr`:
#'   `add_coastline_high_res/add_coastline_high_res_polygon_v7_10.gpkg`, the
#'   ADD high-resolution coastline polygons, which hold land, ice shelf,
#'   ice tongue and rumple classes in one layer. The maps use it when it is on
#'   disk. Point it at a newer release to use one; the caption reads the
#'   version from the file name.
#' * `coastline` and `iceshelves`:
#'   `Continent_ADD/Antarctica_Coastline_med_res_poly.shp` and
#'   `iceshelves/medium_res_IceShelfs.shp`, the medium-resolution layers, used
#'   when the high-resolution file is absent.
#' * `rock`: the high-resolution rock outcrop polygons, from `add_v7_11/` or
#'   from `add_rock_outcrop/`.
#' * `contours`: `add_contours/add_contours_medium_res_v7.3.gpkg`.
#' * `bathymetry`: `ibcso_v2/IBCSO_v2_bed.nc`, the IBCSO v2 bed model, which
#'   is not an ADD layer. The sea floor is tinted from it when it is on disk.
#'
#' Each of those has more than one name across the ADD releases, so the
#' default is the first of a short list of candidates that is on disk. Set
#' the path by hand to pin a release.
#'
#' Elevation comes from the REMA v2.0 32 m tiles of the Polar Geospatial
#' Center, kept as `{row}_{col}_32m_v2.0_dem.tif` in `rema_dir`;
#' [pack_sites()] fetches the tiles a site needs. `rema_1km` is the optional
#' REMA 1 km mosaic, used for a site whose tiles are not on disk.
#'
#' @param cache Folder for everything the package fetches. Defaults to the
#'   package's cache folder from [tools::R_user_dir()].
#' @param rema_dir Folder holding the REMA 32 m tiles. Defaults to
#'   `file.path(cache, "rema")`.
#' @param rema_1km Path to the REMA 1 km mosaic, or `NULL`.
#' @param terrain_cache Folder for the cached terrain rasters. Defaults to
#'   `file.path(cache, "terrain")`.
#' @param spatial Folder holding the ADD layers in the layout described below.
#' @param coastline_hr,coastline,iceshelves,rock,contours Paths to the ADD
#'   layers.
#' @param bathymetry Path to a bed or bathymetry model, or `NULL`.
#' @param output Folder the figure functions write to. While it is `NULL`,
#'   the default, the figure functions return their plots and write nothing.
#' @param terra_tmp Folder for terra's temporary rasters.
#' @param reset Clear every stored setting before applying the arguments.
#' @return The resolved list of paths, invisibly when any argument was given.
#' @examples
#' \dontrun{
#' landfall_paths(
#'   cache   = "D:/landfall",
#'   spatial = "D:/spatial_data",
#'   output  = "D:/landfall/figures"
#' )
#' landfall_paths()$rema_dir
#' landfall_paths(output = NA)   # stop writing figures
#' landfall_paths(reset = TRUE)  # forget everything set this session
#' }
#' @export
landfall_paths <- function(cache = NULL, rema_dir = NULL, rema_1km = NULL, terrain_cache = NULL,
                           spatial = NULL, coastline_hr = NULL, coastline = NULL,
                           iceshelves = NULL, rock = NULL, contours = NULL,
                           bathymetry = NULL, output = NULL,
                           terra_tmp = NULL, reset = FALSE) {
  given <- list(cache = cache, rema_dir = rema_dir, rema_1km = rema_1km,
                terrain_cache = terrain_cache, spatial = spatial, coastline_hr = coastline_hr,
                coastline = coastline, iceshelves = iceshelves, rock = rock,
                contours = contours, bathymetry = bathymetry, output = output,
                terra_tmp = terra_tmp)
  given <- given[!vapply(given, is.null, logical(1))]
  stored <- if (reset) list() else getOption("landfall.paths", list())
  setting <- reset || length(given) > 0
  if (setting) {
    # NA clears a setting. Dropping every NULL above means an omitted argument
    # cannot be told from one passed as NULL, so clearing needs its own value.
    is_clear <- vapply(given, function(v) length(v) == 1 && is.na(v), logical(1))
    stored[names(given)[is_clear]] <- NULL
    keep <- names(given)[!is_clear]
    stored[keep] <- lapply(given[keep], normalizePath, winslash = "/", mustWork = FALSE)
    options(landfall.paths = stored)
  }
  p <- stored
  if (is.null(p[["cache"]])) p[["cache"]] <- tools::R_user_dir("landfall", "cache")
  if (is.null(p[["rema_dir"]])) p[["rema_dir"]] <- file.path(p[["cache"]], "rema")
  if (is.null(p[["terrain_cache"]])) p[["terrain_cache"]] <- file.path(p[["cache"]], "terrain")
  if (!is.null(p[["spatial"]])) {
    under <- function(...) first_existing(p[["spatial"]], c(...))
    if (is.null(p[["coastline_hr"]])) p[["coastline_hr"]] <- under(
      "add_v7_12/add_coastline_high_res_polygon_v7_12.gpkg",
      "add_v7_11/add_coastline_high_res_polygon_v7_11.gpkg",
      "add_coastline_high_res/add_coastline_high_res_polygon_v7_10.gpkg")
    if (is.null(p[["coastline"]]))    p[["coastline"]]    <- under("Continent_ADD/Antarctica_Coastline_med_res_poly.shp")
    if (is.null(p[["iceshelves"]]))   p[["iceshelves"]]   <- under("iceshelves/medium_res_IceShelfs.shp")
    if (is.null(p[["rock"]]))         p[["rock"]]         <- under(
      "add_v7_12/add_rock_outcrop_high_res_polygon_v7_12.gpkg",
      "add_v7_11/add_rock_outcrop_high_res_polygon_v7_11.gpkg",
      "add_rock_outcrop/add_rock_outcrop_high_res_polygon_v7.3.shp")
    if (is.null(p[["contours"]]))     p[["contours"]]     <- under("add_contours/add_contours_medium_res_v7.3.gpkg")
    if (is.null(p[["bathymetry"]]))   p[["bathymetry"]]   <- under("ibcso_v2/IBCSO_v2_bed.nc")
  }
  if (setting && !is.null(p[["terra_tmp"]])) {
    dir.create(p[["terra_tmp"]], recursive = TRUE, showWarnings = FALSE)
    terra::terraOptions(tempdir = p[["terra_tmp"]], progress = 0)
  }
  if (setting) invisible(p) else p
}

# The first of a set of candidate files under a folder that is on disk. When
# none of them is, the first is returned anyway, so the message that follows
# names a path the reader can go and look at. The ADD renames these layers at
# every release, and a default pinned to one release goes quietly missing.
first_existing <- function(dir, rel) {
  p <- file.path(dir, rel)
  hit <- p[file.exists(p)]
  if (length(hit)) hit[1] else p[1]
}

# A path that must be set and must exist, with a message that says what to set.
require_path <- function(paths, field, what) {
  p <- paths[[field]]
  if (is.null(p)) {
    stop(sprintf("%s is needed but landfall_paths()$%s is not set. See ?landfall_paths.",
                 what, field), call. = FALSE)
  }
  if (!file.exists(p)) stop(sprintf("%s not found at %s.", what, p), call. = FALSE)
  p
}

# TRUE when a path is set and the file exists.
has_path <- function(paths, field) {
  !is.null(paths[[field]]) && file.exists(paths[[field]])
}

# TRUE when the layer at a path can actually be opened, which is a different
# question from whether a file sits there. On a machine whose folders sync to
# cloud storage, a layer that has been turned back into a placeholder exists,
# reports its full size, and throws when anything asks for its bytes. A trip
# depends on the second answer, so that is the one check_offline() gives.
# Only the header is read, so the size of the file does not matter.
layer_readable <- function(path) {
  if (is.null(path) || length(path) != 1 || is.na(path) || !file.exists(path)) return(FALSE)
  raster <- grepl("[.](tif|tiff|nc|vrt)$", path, ignore.case = TRUE)
  tryCatch({
    suppressWarnings(if (raster) terra::rast(path) else sf::st_layers(path))
    TRUE
  }, error = function(e) FALSE)
}
