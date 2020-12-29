
#' @title add fork between three nodes, in direction x
#' @param from start node
#' @param to0 the bottom end node
#' @param to1 the top end node
#' @param pos a decimal between 0 and 1 giving the position between `from`, and
#' the closest of `to0` and `to1` at which to split the fork.
#' @param label_from label to draw on the flow out of `from`
#' @param label_to0 label to draw on the flow into `to0`
#' @param label_to1 label to draw on the flow into `to1`
#' @param pos_from a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = bottom and 1 = top
#' @param pos_to0 a decimal between 0 and 1 giving the position to attach the
#' flow to node `to0`, where 0 = bottom and 1 = top
#' @param pos_to1 a decimal between 0 and 1 giving the position to attach the
#' flow to node `to1`, where 0 = bottom and 1 = top
#' @param label_from_pos a decimal between 0 and 1 giving the position along the
#' flow out of `from` to draw `label_from`, where 0 = start and 1 = end
#' @param label_to0_pos a decimal between 0 and 1 giving the position along the
#' flow into `to0` to draw `label_to0`, where 0 = start and 1 = end
#' @param label_to1_pos a decimal between 0 and 1 giving the position along the
#' flow into `to1` to draw `label_to1`, where 0 = start and 1 = end
#' @param label_from_gap distance from the flow out of `from` at which to draw
#'  `label_from` default = 0.05
#' @param label_to0_gap distance from the flow into `to0` at which to draw
#'  `label_to0` default = 0.05
#' @param label_to1_gap distance from the flow into `to1` at which to draw
#'  `label_to1` default = 0.05
#'@param label_from_x x co-ordinate of `label_from` position, overrides use of
#' `label_from_pos` and / or `label_from_gap`
#'@param label_from_y y co-ordinate of `label_from` position, overrides use of
#' `label_from_pos` and / or `label_from_gap`
#'@param label_to0_x x co-ordinate of `label_to0` position, overrides use of
#' `label_to0_pos` and / or `label_to0_gap`
#'@param label_to0_y y co-ordinate of `label_to0` position, overrides use of
#' `label_to0_pos` and / or `label_to0_gap`
#'@param label_to1_x x co-ordinate of `label_to1` position, overrides use of
#' `label_to1_pos` and / or `label_to1_gap`
#'@param label_to1_y y co-ordinate of `label_to1` position, overrides use of
#' `label_to1_pos` and / or `label_to1_gap`
#' @param arr_width width of arrow, defaults to same as [flow()]
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export
forkx <-
  function(from, to0, to1, pos = NULL,
           label_from = NULL, label_to0 = NULL, label_to1 = NULL,
           pos_from = NULL, pos_to0 = NULL, pos_to1 = pos_to0,
           label_from_pos = NULL,
           label_to0_pos = label_from_pos, label_to1_pos = label_to0_pos,
           label_from_gap = NULL,
           label_to0_gap = label_from_gap, label_to1_gap = label_to0_gap,
           label_from_x = NULL, label_to0_x = NULL, label_to1_x = label_to0_x,
           label_from_y = NULL, label_to0_y = NULL, label_to1_y = NULL,
           arr_width = NULL, ...) {

  if (abs(to0$x - from$x) < abs(to1$x - from$x)) {
    nearest <- to0
  } else {
    nearest <- to1
  }
  split <- node(calc_pos(from$x, nearest$x, pos),
                calc_pos(from$y0, from$y1, pos_from),
                r = 0)

  flow(from, split, label_from, label_pos = label_from_pos, arr_width = 0,
       label_gap = label_from_gap, label_x = label_from_x,
       label_y = label_from_y, ...)
  bendy(split, to0, label_to = label_to0, label_to_pos = label_to0_pos,
        label_to_gap = label_to0_gap, label_to_x = label_to0_x,
        label_to_y = label_to0_y, arr_width = arr_width, ...)
  bendy(split, to1, label_to = label_to1, label_to_pos = label_to1_pos,
        label_to_gap = label_to1_gap, label_to_x = label_to1_x,
        label_to_y = label_to1_y, arr_width = arr_width, ...)

  list(x0 = min(from$x0, to0$x0, to1$x0),
       y0 = min(from$y0, to0$y0, to1$y0),
       x1 = max(from$x1, to0$x1, to1$x1),
       y1 = max(from$y1, to0$y1, to1$y1),
       x = split$x, y = split$y)
  }


#' @title add fork between three nodes, in direction y
#' @param from start node
#' @param to0 the left end node
#' @param to1 the right end node
#' @param pos a decimal between 0 and 1 giving the position between `from`, and
#' the closest of `to0` and `to1` at which to split the fork.
#' @param label_from label to draw on the flow out of `from`
#' @param label_to0 label to draw on the flow into `to0`
#' @param label_to1 label to draw on the flow into `to1`
#' @param pos_from a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = left and 1 = right
#' @param pos_to0 a decimal between 0 and 1 giving the position to attach the
#' flow to node `to0`, where 0 = left and 1 = right
#' @param pos_to1 a decimal between 0 and 1 giving the position to attach the
#' flow to node `to1`, where 0 = left and 1 = right
#' @param label_from_pos a decimal between 0 and 1 giving the position along the
#' flow out of `from` to draw `label_from`, where 0 = start and 1 = end
#' @param label_to0_pos a decimal between 0 and 1 giving the position along the
#' flow into `to0` to draw `label_to0`, where 0 = start and 1 = end
#' @param label_to1_pos a decimal between 0 and 1 giving the position along the
#' flow into `to1` to draw `label_to1`, where 0 = start and 1 = end
#' @param label_from_gap distance from the flow out of `from` at which to draw
#'  `label_from` default = 0.05
#' @param label_to0_gap distance from the flow into `to0` at which to draw
#'  `label_to0` default = 0.05
#' @param label_to1_gap distance from the flow into `to1` at which to draw
#'  `label_to1` default = 0.05
#'@param label_from_x x co-ordinate of `label_from` position, overrides use of
#' `label_from_pos` and / or `label_from_gap`
#'@param label_from_y y co-ordinate of `label_from` position, overrides use of
#' `label_from_pos` and / or `label_from_gap`
#'@param label_to0_x x co-ordinate of `label_to0` position, overrides use of
#' `label_to0_pos` and / or `label_to0_gap`
#'@param label_to0_y y co-ordinate of `label_to0` position, overrides use of
#' `label_to0_pos` and / or `label_to0_gap`
#'@param label_to1_x x co-ordinate of `label_to1` position, overrides use of
#' `label_to1_pos` and / or `label_to1_gap`
#'@param label_to1_y y co-ordinate of `label_to1` position, overrides use of
#' `label_to1_pos` and / or `label_to1_gap`
#' @param arr_width width of arrow, defaults to same as [flow()]
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export
forky <-
  function(from, to0, to1, pos = NULL,
           label_from = NULL, label_to0 = NULL, label_to1 = NULL,
           pos_from = NULL, pos_to0 = NULL, pos_to1 = pos_to0,
           label_from_pos = NULL,
           label_to0_pos = label_from_pos, label_to1_pos = label_to0_pos,
           label_from_gap = NULL,
           label_to0_gap = label_from_gap, label_to1_gap = label_to0_gap,
           label_from_x = NULL, label_to0_x = NULL, label_to1_x = NULL,
           label_from_y = NULL, label_to0_y = NULL, label_to1_y = label_to0_y,
           arr_width = NULL, ...) {

    if (abs(to0$y - from$y) < abs(to1$y - from$y)) {
      nearest <- to0
    } else {
      nearest <- to1
    }

    split <- node(calc_pos(from$x0, from$x1, pos_from),
                  calc_pos(from$y, nearest$y, pos),
                  r = 0)

    flow(from, split, label_from, label_pos = label_from_pos, arr_width = 0,
         label_gap = label_from_gap, label_x = label_from_x,
         label_y = label_from_y, ...)
    bendx(split, to0, label_to = label_to0, label_to_pos = label_to0_pos,
          label_to_gap = label_to0_gap, label_to_x = label_to0_x,
          label_to_y = label_to0_y, arr_width = arr_width, ...)
    bendx(split, to1, label_to = label_to1, label_to_pos = label_to1_pos,
          label_to_gap = label_to1_gap, label_to_x = label_to1_x,
          label_to_y = label_to1_y, arr_width = arr_width, ...)

    list(x0 = min(from$x0, to0$x0, to1$x0),
         y0 = min(from$y0, to0$y0, to1$y0),
         x1 = max(from$x1, to0$x1, to1$x1),
         y1 = max(from$y1, to0$y1, to1$y1),
         x = split$x, y = split$y)
  }