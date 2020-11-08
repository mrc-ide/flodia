
#' @title calculates the weighted mean point between two points
#' @param x0 first point
#' @param x1 second point
#' @param pos distance (between 0, 1) from first point, defaults to 0.5
#' @export
calc_pos <- function(x0, x1, pos = NULL) {
  pos <- pos %||% 0.5
  x0 * (1 - pos) + x1 * pos
}
