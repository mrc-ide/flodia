
#' @title calculates the weighted mean point between two points
#' @param x0 first point
#' @param x1 second point
#' @param pos weighting towards first point, defaults to 0.5
#' @export
calc_pos <- function(x0, x1, pos = 0.5) {
  x0 * pos + x1 * (1 - pos)
}
