
#' @title add turn between two nodes in the y-direction
#' @param from start node
#' @param to the end node
#' @param mid the y co-ordinate of the turn
#' @param label_from label to draw on the flow out of `from`
#' @param label_mid label to draw on the `mid` flow
#' @param label_to label to draw on the flow into `to`
#' @param pos_from a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = left and 1 = right
#' @param pos_to a decimal between 0 and 1 giving the position to attach the
#' flow to node `to`, where 0 = left and 1 = right
#' @param label_from_pos a decimal between 0 and 1 giving the position along the
#' flow out of `from` to draw `label_from`, where 0 = bottom and 1 = top
#' @param label_mid_pos a decimal between 0 and 1 giving the position along the
#' `mid` flow to draw `label_mid`, where 0 = left and 1 = right
#' @param label_to_pos a decimal between 0 and 1 giving the position along the
#' flow into `to` to draw `label_to`, where 0 = bottom and 1 = top
#' @param label_from_gap distance from the flow out of `from` at which to draw
#'  `label_from` default = 0.05
#' @param label_mid_gap distance from the `mid` flow at which to draw
#'  `label_mid` default = 0.05
#' @param label_to_gap distance from the flow into `to` at which to draw
#'  `label_to` default = 0.05
#'@param label_from_x x co-ordinate of `label_from` position, overrides use of
#' `label_from_gap`
#'@param label_from_y y co-ordinate of `label_from` position, overrides use of
#' `label_from_pos`
#'@param label_mid_x x co-ordinate of `label_mid` position, overrides use of
#' `label_mid_pos`
#'@param label_mid_y y co-ordinate of `label_mid` position, overrides use of
#' `label_mid_gap`
#'@param label_to_x x co-ordinate of `label_to` position, overrides use of
#' `label_to_gap`
#'@param label_to_y y co-ordinate of `label_to` position, overrides use of
#' `label_to_pos`
#' @param arr_width width of arrow, defaults to same as [flow()]
#' @param name_from internal argument used for informative error messages
#' @param name_to internal argument used for informative error messages
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export
turny <-
  function(from, mid, to, label_from = NULL, label_mid = NULL, label_to = NULL,
           pos_from = NULL, pos_to = NULL,
           label_from_pos = NULL, label_mid_pos = NULL, label_to_pos = NULL,
           label_from_gap = NULL, label_mid_gap = NULL, label_to_gap = NULL,
           label_from_x = NULL, label_mid_x = NULL, label_to_x = NULL,
           label_from_y = NULL, label_mid_y = NULL, label_to_y = NULL,
           arr_width = NULL,
           name_from = deparse(substitute(from)),
           name_to = deparse(substitute(to)),
           ...) {

    assert_no_intersect(from, to, name_from, name_to)
    turn <- node(calc_pos(from$x0, from$x1, pos_from), mid, r = 0)

    flow(from, turn, label = label_from, pos = pos_from,
         label_pos = label_from_pos, label_gap = label_from_gap,
         label_x = label_from_x, label_y = label_from_y,
         arr_width = 0, name_from = name_from, ...)
    bend <- bendx(turn, to, label_from = label_mid, label_to = label_to,
          pos_to = pos_to, label_from_pos = label_mid_pos,
          label_to_pos = label_to_pos, label_from_gap = label_mid_gap,
          label_from_x = label_mid_x, label_from_y = label_mid_y,
          label_to_gap = label_to_gap, label_to_x = label_to_x,
          label_to_y = label_to_y, arr_width = arr_width,
          name_to = name_to, ...)

  list(x0 = min(from$x0, turn$x0, to$x0),
       y0 = min(from$y0, turn$y0, to$y0),
       x1 = max(from$x1, turn$x1, to$x1),
       y1 = max(from$y1, turn$y1, to$y1),
       x = turn$x, y = turn$y,
       from = from, turn1 = turn, turn2 = bend$turn, to = to)
  }


#' @title add turn between two nodes in the x-direction
#' @param from start node
#' @param to the end node
#' @param mid the y co-ordinate of the turn
#' @param label_from label to draw on the flow out of `from`
#' @param label_mid label to draw on the `mid` flow
#' @param label_to label to draw on the flow into `to`
#' @param pos_from a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = left and 1 = right
#' @param pos_to a decimal between 0 and 1 giving the position to attach the
#' flow to node `to`, where 0 = left and 1 = right
#' @param label_from_pos a decimal between 0 and 1 giving the position along the
#' flow out of `from` to draw `label_from`, where 0 = start and 1 = end
#' @param label_mid_pos a decimal between 0 and 1 giving the position along the
#' `mid` flow to draw `label_mid`, where 0 = start and 1 = end
#' @param label_to_pos a decimal between 0 and 1 giving the position along the
#' flow into `to` to draw `label_to`, where 0 = start and 1 = end
#' @param label_from_gap distance from the flow out of `from` at which to draw
#'  `label_from` default = 0.05
#' @param label_mid_gap distance from the `mid` flow at which to draw
#'  `label_mid` default = 0.05
#' @param label_to_gap distance from the flow into `to` at which to draw
#'  `label_to` default = 0.05
#'@param label_from_x x co-ordinate of `label_from` position, overrides use of
#' `label_from_pos` and / or `label_from_gap`
#'@param label_from_y y co-ordinate of `label_from` position, overrides use of
#' `label_from_pos` and / or `label_from_gap`
#'@param label_mid_x x co-ordinate of `label_mid` position, overrides use of
#' `label_mid_pos` and / or `label_mid_gap`
#'@param label_mid_y y co-ordinate of `label_mid` position, overrides use of
#' `label_mid_pos` and / or `label_mid_gap`
#'@param label_to_x x co-ordinate of `label_to` position, overrides use of
#' `label_to_pos` and / or `label_to_gap`
#'@param label_to_y y co-ordinate of `label_to` position, overrides use of
#' `label_to_pos` and / or `label_to_gap`
#' @param arr_width width of arrow, defaults to same as [flow()]
#' @param name_from internal argument used for informative error messages
#' @param name_to internal argument used for informative error messages
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export
turnx <-
  function(from, mid, to, label_from = NULL, label_mid = NULL, label_to = NULL,
           pos_from = NULL, pos_to = NULL,
           label_from_pos = NULL, label_mid_pos = NULL, label_to_pos = NULL,
           label_from_gap = NULL, label_mid_gap = NULL, label_to_gap = NULL,
           label_from_x = NULL, label_mid_x = NULL, label_to_x = NULL,
           label_from_y = NULL, label_mid_y = NULL, label_to_y = NULL,
           arr_width = NULL,
           name_from = deparse(substitute(from)),
           name_to = deparse(substitute(to)),
           ...) {

    assert_no_intersect(from, to, name_from, name_to)
    turn <- node(mid, calc_pos(from$y0, from$y1, pos_from), r = 0)

    flow(from, turn, label = label_from, pos = pos_from,
         label_pos = label_from_pos, label_gap = label_from_gap, arr_width = 0,
         label_x = label_from_x, label_y = label_from_y, name_from = name_from,
         ...)
    bend <- bendy(turn, to, label_from = label_mid, label_to = label_to,
          pos_to = pos_to, label_from_pos = label_mid_pos,
          label_to_pos = label_to_pos, label_from_gap = label_mid_gap,
          label_from_x = label_mid_x, label_from_y = label_mid_y,
          label_to_gap = label_to_gap, label_to_x = label_to_x,
          label_to_y = label_to_y, arr_width = arr_width,
          name_to = name_to, ...)

    list(x0 = min(from$x0, turn$x0, to$x0),
         y0 = min(from$y0, turn$y0, to$y0),
         x1 = max(from$x1, turn$x1, to$x1),
         y1 = max(from$y1, turn$y1, to$y1),
         x = turn$x, y = turn$y,
         from = from, turn1 = turn, turn2 = bend$turn, to = to)
  }
