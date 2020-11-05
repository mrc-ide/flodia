
#' @title start a flodia plot
#' @param x0 left-most x
#' @param y0 bottom-most y
#' @param x1 right-most x
#' @param y1 top-most y
#' @param filepath desired location of output eg "test.png"
#' @param width width in pixels of output (height is calculated automatically
#'  based on plot dimensions)
#' @param res resolution of plot default = 200 dpi
#' @param oma outer margin of plot, default is 1
#' @export
#' @importFrom grDevices png
#' @importFrom graphics par
#' @importFrom withr with_par
start_plot <- function(x0, y0, x1, y1, filepath, width = 1200, res = 200,
                       oma = c(1, 1, 1, 1)) {
  height <- round(width * (y1 - y0) / (x1 - x0))
  png(filepath, width, height, res = res)
  withr::with_par(mar = rep(0, 4), mgp = rep(0, 3), bty = "n", oma = oma)
  plot(0, 0, type = "n", xlim = c(x0, x1), ylim = c(y0, y1), axes = FALSE)
  message("Remember to close with dev.off()!")
}
