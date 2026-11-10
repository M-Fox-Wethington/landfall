# --- ADD coastline, contours and rock -------------------------------------------

# The first layer of a data source that holds features, so a GeoPackage with a
# styles table beside its data reads without a warning.
data_layer <- function(path) {
  info <- sf::st_layers(path)
  keep <- !is.na(info$geomtype) & !vapply(info$geomtype, function(g) all(is.na(g)), logical(1))
  info$name[keep][1]
}

# The CRS a layer is stored in, or NA when the file does not say.
layer_crs <- function(path, layer) {
  info <- sf::st_layers(path)
  i <- match(layer, info$name)
  if (is.na(i) || is.null(info$crs)) return(sf::NA_crs_)
  crs <- info$crs[[i]]
  if (is.null(crs)) return(sf::NA_crs_)
  crs <- sf::st_crs(crs)
  if (is.na(crs)) sf::NA_crs_ else crs
}

# "v7.12" from a file named ..._v7_12.gpkg or ..._v7.12.shp, or NA when the
# name carries no version.
add_version <- function(path) {
  base <- basename(path)
  m <- regmatches(base, regexpr("[vV][0-9]+[._][0-9]+", base))
  if (!length(m)) return(NA_character_)
  tolower(sub("_", ".", m))
}

# What to call the coastline in a caption. The version is read from the file
# that was actually loaded, so pointing the setting at a newer release changes
# what the map says rather than leaving a stale number behind.
coast_source_label <- function(path, resolution) {
  v <- add_version(path)
  if (is.na(v)) paste0(resolution, " coastline (no version in the file name)")
  else paste(resolution, "coastline", v)
}

# Reads a vector layer within the window, drops empty and invalid geometries,
# and transforms it to the window's CRS.
read_in_window <- function(path, window, layer = NULL) {
  if (is.null(layer)) layer <- data_layer(path)
  src <- layer_crs(path, layer)
  # A spatial filter is read in the coordinates of the file it is applied to,
  # so it is written in that file's own CRS. Sending EPSG:3031 coordinates to a
  # layer stored in anything else would return the wrong features, or none, and
  # would say nothing about it.
  wkt <- if (is.na(src)) {
    window$wkt_3031
  } else {
    sf::st_as_text(sf::st_geometry(sf::st_transform(window$padded, src)))
  }
  x <- sf::st_read(path, layer = layer, quiet = TRUE, wkt_filter = wkt)
  if (!nrow(x)) return(sf::st_transform(x, window$crs))
  x <- x[!sf::st_is_empty(x), ]
  x <- sf::st_make_valid(sf::st_zm(x))
  sf::st_transform(x, window$crs)
}

# Read a layer the map can do without, and say so when it cannot be read.
#
# A file can be on disk, report its full size, and still throw when anything
# asks for its bytes: that is what a layer looks like once cloud storage has
# turned it back into a placeholder. The map is then drawn without it, and the
# reason is said out loud rather than left to look like an empty window.
optional_layer <- function(path, window, what) {
  tryCatch(read_in_window(path, window), error = function(e) {
    message(sprintf("The %s layer at %s could not be read (%s); the map is drawn without it.",
                    what, path, conditionMessage(e)))
    NULL
  })
}

# The column that classifies the high-resolution coastline polygons.
coast_class_column <- function(x) {
  for (nm in c("surface", "SURFACE", "class", "CLASS", "type")) if (nm %in% names(x)) return(nm)
  NULL
}

#' Land and ice shelves within a window
#'
#' Reads the ADD coastline within the window. With the high-resolution
#' GeoPackage on disk, its `surface` classes are split so that land and ice
#' rumples count as land and ice shelves and ice tongues count as shelves.
#' Otherwise the medium-resolution coastline and ice-shelf shapefiles are read.
#'
#' @param window A [site_window()].
#' @param paths The list from [landfall_paths()].
#' @return A list with `land` and `shelves`, both sf objects in the window's
#'   CRS, `source`, which names the coastline layer and the version read from
#'   its file name, and `files`, the paths that were read.
#' @export
load_coast <- function(window, paths = landfall_paths()) {
  if (has_path(paths, "coastline_hr")) {
    x <- read_in_window(paths$coastline_hr, window)
    col <- coast_class_column(x)
    if (is.null(col) || !nrow(x)) {
      land <- x
      shelves <- x[0, ]
    } else {
      cls <- tolower(x[[col]])
      shelves <- x[grepl("shelf|tongue", cls), ]
      land    <- x[!grepl("shelf|tongue", cls), ]
    }
    return(list(land = land, shelves = shelves,
                source = coast_source_label(paths$coastline_hr, "high-resolution"),
                files = paths$coastline_hr))
  }
  coast <- require_path(paths, "coastline", "The ADD coastline layer")
  shelf <- require_path(paths, "iceshelves", "The ADD ice-shelf layer")
  list(land = read_in_window(coast, window), shelves = read_in_window(shelf, window),
       source = coast_source_label(coast, "medium-resolution"),
       files = c(coast, shelf))
}

#' Contours within a window
#'
#' @param window A [site_window()].
#' @param levels The contour levels to keep, in meters.
#' @param major Every level that is a multiple of this is a major contour.
#' @param paths The list from [landfall_paths()].
#' @return An sf object of lines with `level` and `is_major`, or `NULL` when
#'   the contour layer is not on disk.
#' @export
load_contours <- function(window, levels = seq(50, 2500, by = 50), major = 200,
                          paths = landfall_paths()) {
  if (!has_path(paths, "contours")) return(NULL)
  ct <- optional_layer(paths$contours, window, "contour")
  if (is.null(ct) || !nrow(ct)) return(NULL)
  ct <- ct |> dplyr::rename(level = contour) |> mutate(level = as.numeric(level)) |>
    filter(level %in% levels)
  ct$is_major <- ct$level %% major == 0
  ct
}

#' Rock outcrops within a window
#'
#' @param window A [site_window()].
#' @param paths The list from [landfall_paths()].
#' @return An sf object of polygons, or `NULL` when the layer is not on disk
#'   or holds nothing in the window.
#' @export
load_rock <- function(window, paths = landfall_paths()) {
  if (!has_path(paths, "rock")) return(NULL)
  rock <- optional_layer(paths$rock, window, "rock outcrop")
  if (is.null(rock) || !nrow(rock)) return(NULL)
  rock
}
