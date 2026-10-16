# ------------------------------------------------------------------------------
# BRI house style
#
# COLORS, theme_bri() and geom_halo_points() are copied from the design-library
# repository (templates/r/bri_figure_style.R, compiled from the bri-figure-style
# skill) on 2026-10-16, so the package installs without that repository. Keep
# them in step with the skill when either changes. The same copy sits in the
# frazil package.
# ------------------------------------------------------------------------------

COLORS <- list(
  parchment    = "#f5f2ed",
  ocean        = "#8facc0",
  land         = "#d8dce0",
  coastline    = "#3d5a6e",
  braun_white  = "#F2F0EC",
  title        = "#1D1D1B",
  subtitle     = "#323733",
  muted        = "#8C8C84",
  grid         = "#DADCCF",
  orange       = "#CC6324",
  slate        = "#3B4B59",
  yellow       = "#ECC046",
  red          = "#AF2E1B",
  sand         = "#BFA07A",
  navy         = "#003452",
  brown        = "#5E483E",
  print_blue   = "#285388",
  braun_orange = "#F3672B",
  lemon        = "#F8E538",
  teak         = "#AF6D40",
  dark_brown   = "#392F2D",
  olive        = "#545850",
  aluminum     = "#B1AFAD",
  cream        = "#D9C3B0",
  paper        = "#E2D9CE"
)

#' The BRI color palette
#'
#' The Dieter Rams (Braun) palette of the BRI figure style, as a named list of
#' hex colors.
#'
#' @format A named list of 25 hex color strings.
#' @export
bri_colors <- COLORS

#' The BRI ggplot2 theme
#'
#' @param base_size Base font size in points.
#' @param ground Background color.
#' @return A ggplot2 theme.
#' @export
theme_bri <- function(base_size = 11, ground = "white") {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = ground, color = NA),
      panel.background  = ggplot2::element_rect(fill = ground, color = NA),
      panel.grid.major  = ggplot2::element_line(color = COLORS$grid, linewidth = 0.2),
      panel.grid.minor  = ggplot2::element_blank(),
      plot.title        = ggplot2::element_text(size = 16, face = "bold",
                                                color = COLORS$title,
                                                margin = ggplot2::margin(b = 4)),
      plot.subtitle     = ggplot2::element_text(size = 10, color = COLORS$subtitle,
                                                margin = ggplot2::margin(b = 8)),
      plot.caption      = ggplot2::element_text(size = 7, color = COLORS$muted,
                                                hjust = 1, margin = ggplot2::margin(t = 8),
                                                lineheight = 1.3),
      axis.title        = ggplot2::element_text(size = 9, color = COLORS$subtitle),
      axis.text         = ggplot2::element_text(size = 8, color = COLORS$muted),
      legend.title      = ggplot2::element_text(size = 9, face = "bold",
                                                color = COLORS$title),
      legend.text       = ggplot2::element_text(size = 8, color = COLORS$subtitle),
      legend.background = ggplot2::element_rect(fill = ggplot2::alpha(ground, 0.85),
                                                color = NA),
      strip.text        = ggplot2::element_text(size = 10, face = "bold",
                                                color = COLORS$title),
      plot.margin       = ggplot2::margin(12, 16, 10, 12)
    )
}

#' Site markers with a white halo
#'
#' Three stacked `geom_sf()` point layers: a white halo, the colored point and
#' a faint dark outline ring.
#'
#' @param data An sf object of points.
#' @param color Point color.
#' @param size Point size.
#' @return A list of three ggplot2 layers.
#' @export
geom_halo_points <- function(data, color = COLORS$orange, size = 3.0) {
  list(
    ggplot2::geom_sf(data = data, color = "white", size = size + 1.8, shape = 16),
    ggplot2::geom_sf(data = data, color = color, size = size, shape = 16),
    ggplot2::geom_sf(data = data, color = ggplot2::alpha(COLORS$title, 0.45),
                     shape = 1, size = size, stroke = 0.32)
  )
}

# --- Map colors: the BRI neighborhood-map relief --------------------------------
MAP_PAPER     <- "#f5f2ed"
MAP_OCEAN     <- "#8facc0"
MAP_COAST     <- "#3d5a6e"
MAP_TEXT      <- "#2c3e50"
SHELF_FILL    <- "#d4e4ef"
SHELF_BORDER  <- "#a8c0d2"
ROCK_FILL     <- "#806b5a"
ROCK_BORDER   <- "#594536"
SITE_COLOR    <- COLORS$orange

# The hypsometric ice ramp of the neighborhood maps: eight steps from near
# white at sea level to a deep gray-blue above 2400 m.
HYPSO_PALETTE <- c("#f4f8fb", "#e3edf3", "#cfdde7", "#bdcedc",
                   "#a9bdcc", "#94aabb", "#7d97a9", "#677e8e")
HYPSO_BREAKS  <- c(0, 200, 400, 600, 800, 1200, 1800, 2400)

# geom_text() size from a size in points.
pt <- function(x) x / ggplot2::.pt

# Species labels for the colony panels.
species_label <- function(species_id) {
  labels <- c(ADPE = paste0("Ad", E_ACUTE, "lie penguin"), CHPE = "Chinstrap penguin",
              EMPE = "Emperor penguin", GEPE = "Gentoo penguin", KIPE = "King penguin",
              MCPE = "Macaroni penguin", UNPE = "Unidentified penguin")
  out <- unname(labels[species_id])
  out[is.na(out)] <- species_id[is.na(out)]
  out
}

# The species name as it reads inside a sentence: lower case, except the
# proper name in "Adelie penguin".
species_phrase <- function(species_id) {
  x <- tolower(species_label(species_id))
  sub(paste0("^ad", E_ACUTE, "lie"), paste0("Ad", E_ACUTE, "lie"), x)
}
