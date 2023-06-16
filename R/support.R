
#' @title calculates the weighted mean point between two points
#' @param x0 first point
#' @param x1 second point
#' @param pos distance (between 0, 1) from left / bottom point, defaults to 0.5
#' @export
calc_pos <- function(x0, x1, pos = NULL) {
  pos <- pos %||% 0.5
  min(x0, x1) * (1 - pos) + max(x0, x1) * pos
}

#' @title finds centre of a flodia object
#' @param object list containing at list x0, x1, y0 and y1
#' @export
find_centre <- function(object) {
  object$x <- calc_pos(object$x0, object$x1)
  object$y <- calc_pos(object$y0, object$y1)
  object
}
