#' @title plot a flodia
#' @param f flodia plot function
#' @param oma single numeric outer margin of plot, default is 1
#' @param args list of named arguments to `f`
#' @importFrom withr with_par
#' @export
flodia <- function(f, oma = 0.1, args = list()) {

  z <- flodia_null(f, oma, args)

  withr::with_par(new = list(mar = rep(0, 4), mgp = rep(0, 3), bty = "n",
                             oma = rep(0, 4)),
                  code = {
                    plot(0, 0, type = "n", xlim = c(z$x0, z$x1),
                         ylim = c(z$y0, z$y1),
                         axes = FALSE)
                    do.call(f, args)
                  })

  z
}

#' @title save a flodia
#' @param f flodia plot function
#' @param filepath desired location of output eg "test.png"
#' @param width width in pixels of output (height is calculated automatically
#'  based on plot dimensions)
#' @param res resolution of plot default = 200 dpi
#' @param oma single numeric outer margin of plot, default is 0.1
#' @param args list of named arguments to `f`
#' @param ... other arguments to png()
#' @export
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
flodia_png <- function(f, filepath, width = 1200, res = 200, oma = 0.1,
                       args = list(), ...) {

  # extract co-ordinates of flow diagram
  z <- flodia_null(f, oma, args)

  height <- round(width * (z$y1 - z$y0) / (z$x1 - z$x0))
  png(filepath, width, height, res = res, ...)
  flodia(f, oma, args)
  dev.off()
}


#' @title run a flodia without plotting (e.g to extract co-ordinates)
#' @param f flodia plot function
#' @param oma single numeric outer margin of plot, default is 0.1
#' @param args list of named arguments to `f`
#' @importFrom grDevices pdf
#' @importFrom graphics plot.new
#' @export
flodia_null <- function(f, oma = 0.1, args = list()) {

  pdf(NULL)
  plot.new()
  z <- do.call(f, args)
  dev.off()

  z$x0 <- z$x0 - oma
  z$x1 <- z$x1 + oma
  z$y0 <- z$y0 - oma
  z$y1 <- z$y1 + oma

  z
}
