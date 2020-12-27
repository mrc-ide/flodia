#' @title add outflow to a node, in direction x
#' @param from start node
#' @param length single numeric specifying length, positive will result in a
#' flow from left to right, negative from right to left
#' @param pos a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = bottom and 1 = top, default = 0.5
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export

outflowx <- function(from, length, pos = NULL, ...) {

  x <- ifelse(length > 0, from$x1, from$x0) + length

  to <- node(x, calc_pos(from$y0, from$y1, pos), r = 0)

  f <- flow(from, to, ...)

  list(x0 = min(to$x0, from$x0), y0 = min(to$y0, from$y0),
       x1 = max(to$x1, from$x1), y1 = max(to$y1, from$y1),
       x = f$x, y = f$y)
}

#' @title add outflow to a node, in direction y
#' @param from end node
#' @param length single numeric specifying length, positive will result in a
#' flow from bottom to top, negative from top to bottom
#' @param pos a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = left and 1 = right, default = 0.5
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export

outflowy <- function(from, length, pos = NULL, ...) {

  y <- ifelse(length > 0, from$y1, from$y0) + length

  to <- node(calc_pos(from$x0, from$x1, pos), y, r = 0)

  f <- flow(from, to, ...)

  list(x0 = min(to$x0, from$x0), y0 = min(to$y0, from$y0),
       x1 = max(to$x1, from$x1), y1 = max(to$y1, from$y1),
       x = f$x, y = f$y)
}
