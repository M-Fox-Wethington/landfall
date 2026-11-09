#' Write a figure as a PNG and a PDF
#'
#' Writes `name.png` through ragg at `dpi` and `name.pdf` through `cairo_pdf`
#' into `out_dir`. The figure functions call this themselves and return the
#' paths it wrote.
#'
#' Existing files of the same name are left alone and an error says which they
#' are, so one observation's report cannot quietly replace another's. Pass
#' `overwrite = TRUE` when replacing them is what you want.
#'
#' @param plot A ggplot or cowplot object.
#' @param name File name without extension.
#' @param out_dir Folder to write into. `NULL` writes nothing.
#' @param width,height Size in inches.
#' @param dpi Resolution of the PNG.
#' @param bg Background color.
#' @param overwrite Write over files of the same name that already exist.
#' @return The two paths, invisibly, or `NULL` when nothing was written.
#' @export
save_figure <- function(plot, name, out_dir = landfall_paths()$output, width = 16, height = 9,
                        dpi = 300, bg = "white", overwrite = FALSE) {
  if (is.null(out_dir)) return(invisible(NULL))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  png_path <- file.path(out_dir, paste0(name, ".png"))
  pdf_path <- file.path(out_dir, paste0(name, ".pdf"))
  if (!overwrite) {
    existing <- c(png_path, pdf_path)[file.exists(c(png_path, pdf_path))]
    if (length(existing)) {
      stop(sprintf(paste("%s already exists. Give a different name, or pass",
                         "overwrite = TRUE to replace it."),
                   paste(basename(existing), collapse = " and ")), call. = FALSE)
    }
  }
  ggplot2::ggsave(png_path, plot, width = width, height = height, units = "in",
                  dpi = dpi, device = ragg::agg_png, bg = bg)
  ggplot2::ggsave(pdf_path, plot, width = width, height = height, units = "in",
                  device = grDevices::cairo_pdf, bg = bg)
  message("Saved ", png_path, " and ", basename(pdf_path))
  invisible(c(png = png_path, pdf = pdf_path))
}
