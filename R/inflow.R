#' @title add inflow to a node, in direction x
#' @param to end node
#' @param length single numeric specifying length, positive will result in a
#' flow from left to right, negative from right to left
#' @param pos a decimal between 0 and 1 giving the position to attach the
#' flow to node `to`, where 0 = bottom and 1 = top, default = 0.5
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export

inflowx <- function(to, length, pos = NULL, ...) {

  x <- ifelse(length > 0, to$x0, to$x1) - length

  from <- node(x, calc_pos(to$y0, to$y1, pos), r = 0)

  f <- flow(from, to, ...)

  list(x0 = min(to$x0, from$x0), y0 = min(to$y0, from$y0),
       x1 = max(to$x1, from$x1), y1 = max(to$y1, from$y1),
       x = f$x, y = f$y)
}

#' @title add inflow to a node, in direction y
#' @param to end node
#' @param length single numeric specifying length, positive will result in a
#' flow from bottom to top, negative from top to bottom
#' @param pos a decimal between 0 and 1 giving the position to attach the
#' flow to node `to`, where 0 = left and 1 = right, efault = 0.5
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export

inflowy <- function(to, length, pos = NULL, ...) {

  y <- ifelse(length > 0, to$y0, to$y1) - length

  from <- node(calc_pos(to$x0, to$x1, pos), y, r = 0)

  f <- flow(from, to, ...)

  list(x0 = min(to$x0, from$x0), y0 = min(to$y0, from$y0),
       x1 = max(to$x1, from$x1), y1 = max(to$y1, from$y1),
       x = f$x, y = f$y)
}
