# --- ADD coastline, contours and rock -------------------------------------------

# The first layer of a data source that holds features, so a GeoPackage with a
# styles table beside its data reads without a warning.
data_layer <- function(path) {
  info <- sf::st_layers(path)
  keep <- !is.na(info$geomtype) & !vapply(info$geomtype, function(g) all(is.na(g)), logical(1))
  info$name[keep][1]
}

# Reads a vector layer within the window, drops empty and invalid geometries,
# and transforms it to the window's CRS.
read_in_window <- function(path, window, layer = NULL) {
  if (is.null(layer)) layer <- data_layer(path)
  x <- sf::st_read(path, layer = layer, quiet = TRUE, wkt_filter = window$wkt_3031)
  if (!nrow(x)) return(sf::st_transform(x, window$crs))
  x <- x[!sf::st_is_empty(x), ]
  x <- sf::st_make_valid(sf::st_zm(x))
  sf::st_transform(x, window$crs)
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
#'   CRS, and `source`, which names the coastline layer used.
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
    return(list(land = land, shelves = shelves, source = "high-resolution coastline v7.10"))
  }
  coast <- require_path(paths, "coastline", "The ADD coastline layer")
  shelf <- require_path(paths, "iceshelves", "The ADD ice-shelf layer")
  list(land = read_in_window(coast, window), shelves = read_in_window(shelf, window),
       source = "medium-resolution coastline v7.3")
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
  ct <- read_in_window(paths$contours, window)
  if (!nrow(ct)) return(NULL)
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
  rock <- tryCatch(read_in_window(paths$rock, window), error = function(e) NULL)
  if (is.null(rock) || !nrow(rock)) return(NULL)
  rock
}
