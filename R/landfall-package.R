#' landfall: site maps and colony records for Antarctic expeditions
#'
#' Draws a relief map of the surroundings of any Antarctic site and places a
#' new penguin colony count against the site's record in MAPPPD, the Mapping
#' Application for Penguin Populations and Projected Dynamics. A site is given
#' by its four-letter MAPPPD code or by a name and coordinates, the map covers
#' a chosen radius, and [site_report()] renders a figure with the map, the
#' long-term record and how the new count compares with earlier seasons.
#'
#' Every input lives in a local folder set with [landfall_paths()], and
#' [pack_sites()] fetches what a list of sites needs before a trip, so the
#' figures render with no network connection.
#'
#' @keywords internal
"_PACKAGE"

#' @import ggplot2
#' @importFrom dplyr filter mutate select arrange group_by summarise ungroup
#'   left_join bind_rows distinct n
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom utils head tail untar
#' @importFrom grDevices col2rgb
#' @importFrom grid unit
#' @importFrom tidyterra geom_spatraster_rgb
NULL

utils::globalVariables(c(
  "site_id", "species_id", "season", "count", "type", "accuracy", "year",
  "common_name", "site_name", "region", "latitude", "longitude", "peak",
  "lo", "hi", "focal", "level", "contour", "surface", "geometry", "name",
  "label", "x", "y", "n_counts", "dist_km", "code", "shape_type"
))
