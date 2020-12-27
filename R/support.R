
#' @title calculates the weighted mean point between two points
#' @param x0 first point
#' @param x1 second point
#' @param pos distance (between 0, 1) from left / bottom point, defaults to 0.5
#' @export
calc_pos <- function(x0, x1, pos = NULL) {
  pos <- pos %||% 0.5
  min(x0, x1) * (1 - pos) + max(x0, x1) * pos
}
