#' @title plot a flodia
#' @param f flodia plot function
#' @param oma single numeric outer margin of plot, default is 1
#' @importFrom withr with_par
#' @export
flodia <- function(f, oma = 0.1) {

  z <- flodia_null(f)
  xlim <- c(z$x0 - oma, z$x1 + oma)
  ylim <- c(z$y0 - oma, z$y1 + oma)

  withr::with_par(new = list(mar = rep(0, 4), mgp = rep(0, 3), bty = "n",
                             oma = rep(0, 4)),
                  code = {
                    plot(0, 0, type = "n", xlim = xlim, ylim = ylim,
                         axes = FALSE)
                    f()
                  })

  list(x0 = xlim[1], y0 = ylim[1], x1 = xlim[2], y1 = ylim[2])
}

#' @title save a flodia
#' @param f flodia plot function
#' @param filepath desired location of output eg "test.png"
#' @param width width in pixels of output (height is calculated automatically
#'  based on plot dimensions)
#' @param res resolution of plot default = 200 dpi
#' @param oma single numeric outer margin of plot, default is 0.1
#' @export
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
flodia_png <- function(f, filepath, width = 1200, res = 200, oma = 0.1, ...) {

  # extract co-ordinates of flow diagram
  z <- flodia_null(f)

  height <- round(width * (z$y1 - z$y0) / (z$x1 - z$x0))
  png(filepath, width, height, res = res, ...)
  flodia(f, oma)
  dev.off()
}


#' @title run a flodia without plotting (e.g to extract co-ordinates)
#' @param f flodia plot function
#' @importFrom grDevices pdf
#' @importFrom graphics plot.new
#' @export
flodia_null <- function(f) {

  pdf(NULL)
  plot.new()
  z <- f()
  dev.off()

  z
}
