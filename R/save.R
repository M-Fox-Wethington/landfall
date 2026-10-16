#' Write a figure as a PNG and a PDF
#'
#' Writes `name.png` through ragg at `dpi` and `name.pdf` through `cairo_pdf`
#' into `out_dir`. The figure functions call this themselves.
#'
#' @param plot A ggplot or cowplot object.
#' @param name File name without extension.
#' @param out_dir Folder to write into. `NULL` writes nothing.
#' @param width,height Size in inches.
#' @param dpi Resolution of the PNG.
#' @param bg Background color.
#' @return The two paths, invisibly, or `NULL` when nothing was written.
#' @export
save_figure <- function(plot, name, out_dir = landfall_paths()$output, width = 16, height = 9,
                        dpi = 300, bg = "white") {
  if (is.null(out_dir)) return(invisible(NULL))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  png_path <- file.path(out_dir, paste0(name, ".png"))
  pdf_path <- file.path(out_dir, paste0(name, ".pdf"))
  ggplot2::ggsave(png_path, plot, width = width, height = height, units = "in",
                  dpi = dpi, device = ragg::agg_png, bg = bg)
  ggplot2::ggsave(pdf_path, plot, width = width, height = height, units = "in",
                  device = grDevices::cairo_pdf, bg = bg)
  message("Saved ", png_path, " and ", basename(pdf_path))
  invisible(c(png = png_path, pdf = pdf_path))
}
