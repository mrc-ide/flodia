#' @title add bend between two nodes, in direction x then y
#' @param from start node
#' @param to end node
#' @param label_from label to draw on the flow out of `from`
#' @param label_to label to draw on the flow into `to`
#' @param pos_from a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = bottom and 1 = top
#' @param pos_to a decimal between 0 and 1 giving the position to attach the
#' flow to node `to`, where 0 = left and 1 = right
#' @param label_from_pos a decimal between 0 and 1 giving the position along the
#' flow out of `from` to draw `label_from`, where 0 = start and 1 = end
#' @param label_to_pos a decimal between 0 and 1 giving the position along the
#' flow into `to` to draw `label_to`, where 0 = start and 1 = end
#' @param label_from_gap distance from the flow out of `from` at which to draw
#'  `label_from` default = 0.05
#' @param label_to_gap distance from the flow into `to` at which to draw
#'  `label_to` default = 0.05
#'@param label_from_x x co-ordinate of `label_from` position, overrides use of
#' `label_from_pos` and / or `label_from_gap`
#'@param label_from_y y co-ordinate of `label_from` position, overrides use of
#' `label_from_pos` and / or `label_from_gap`
#'@param label_to_x x co-ordinate of `label_to` position, overrides use of
#' `label_to_pos` and / or `label_to_gap`
#'@param label_to_y y co-ordinate of `label_to` position, overrides use of
#' `label_to_pos` and / or `label_to_gap`
#' @param arr_width width of arrow, defaults to same as [flow()]
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export

bendx <- function(from, to, label_from = NULL, label_to = NULL, pos_from = NULL,
                  pos_to = NULL, label_from_pos = NULL, label_to_pos = NULL,
                  label_from_gap = NULL, label_to_gap = NULL,
                  label_from_x = NULL, label_to_x = NULL,
                  label_from_y = NULL, label_to_y = NULL,
                  arr_width = NULL, ...) {

  mid <- node(x = calc_pos(to$x0, to$x1, pos_to),
                      y = calc_pos(from$y0, from$y1, pos_from),
                      r = 0)

  flow(from, mid, pos = pos_from, arr_width = 0, label = label_from,
       label_pos = label_from_pos, label_gap = label_from_gap,
       label_x = label_from_x, label_y = label_from_y, ...)
  flow(mid, to, label = label_to, label_pos = label_to_pos,
       label_gap = label_to_gap, arr_width = arr_width,
       label_x = label_to_x, label_y = label_to_y, ...)

  list(x0 = from$x1, y0 = mid$y, x1 = mid$x, y1 = to$y0, x = mid$x, y = mid$y)
}


#' @title add bend between two nodes, in direction y then x
#' @param from start node
#' @param to end node
#' @param label_from label to draw on the flow out of `from`
#' @param label_to label to draw on the flow into `to`
#' @param pos_from a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = left and 1 = right
#' @param pos_to a decimal between 0 and 1 giving the position to attach the
#' flow to node `to`, where 0 = bottom and 1 = top
#' @param label_from_pos a decimal between 0 and 1 giving the position along the
#' flow out of `from` to draw `label_from`, where 0 = start and 1 = end
#' @param label_to_pos a decimal between 0 and 1 giving the position along the
#' flow into `to` to draw `label_to`, where 0 = start and 1 = end
#' @param label_from_gap distance from the flow out of `from` at which to draw
#'  `label_from` default = 0.05
#' @param label_to_gap distance from the flow into `to` at which to draw
#'  `label_to` default = 0.05
#'@param label_from_x x co-ordinate of `label_from` position, overrides use of
#' `label_from_pos` and / or `label_from_gap`
#'@param label_from_y y co-ordinate of `label_from` position, overrides use of
#' `label_from_pos` and / or `label_from_gap`
#'@param label_to_x x co-ordinate of `label_to` position, overrides use of
#' `label_to_pos` and / or `label_to_gap`
#'@param label_to_y y co-ordinate of `label_to` position, overrides use of
#' `label_to_pos` and / or `label_to_gap`
#' @param arr_width width of arrow, defaults to same as [flow()]
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export

bendy <- function(from, to, label_from = NULL, label_to = NULL, pos_from = NULL,
                  pos_to = NULL, label_from_pos = NULL, label_to_pos = NULL,
                  label_from_gap = NULL, label_to_gap = NULL, arr_width = NULL,
                  label_from_x = NULL, label_to_x = NULL,
                  label_from_y = NULL, label_to_y = NULL,
                  ...)  {

  mid <- node(x = calc_pos(from$x0, from$x1, pos_from),
                      y = calc_pos(to$y0, to$y1, pos_to),
                      r = 0)
  flow(from, mid, pos = pos_from, label = label_from,
       label_pos = label_from_pos, arr_width = 0,
       label_gap = label_from_gap,
       label_x = label_from_x, label_y = label_from_y, ...)
  flow(mid, to, label = label_to, label_pos = label_to_pos,
       label_gap = label_to_gap, arr_width = arr_width,
       label_x = label_to_x, label_y = label_to_y, ...)

  list(x0 = mid$x, y0 = from$y1, x1 = to$x0, y1 = mid$y, x = mid$x, y = mid$y)
}
