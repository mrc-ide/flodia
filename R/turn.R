#' @title add turn between two nodes in the x-direction
#' @param from start node
#' @param to the end node
#' @param mid_x the x co-ordinate of the mid flow
#' @param label label to draw on the`mid flow
#' @param pos_from a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = left and 1 = right
#' @param pos_to a decimal between 0 and 1 giving the position to attach the
#' flow to node `to`, where 0 = left and 1 = right
#' @param label_pos a decimal between 0 and 1 giving the position along the
#' `mid` flow to draw `label`, where 0 = start and 1 = end
#' @param label_gap distance from the `mid` flow at which to draw
#'  `label` default = 0.05
#'@param label_x x co-ordinate of `label` position, overrides use of
#' `label_pos` and / or `label_gap`
#'@param label_y y co-ordinate of `label` position, overrides use of
#' `label_pos` and / or `label_gap`
#' @param arr_width width of arrow, defaults to same as [flow()]
#' @param name_from internal argument used for informative error messages
#' @param name_to internal argument used for informative error messages
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export
turnx <-
  function(from, mid_x, to, label = NULL,
           pos_from = NULL, pos_to = NULL,
           label_pos = NULL, label_gap = NULL, label_x = NULL, label_y = NULL,
           arr_width = NULL,
           name_from = deparse(substitute(from)),
           name_to = deparse(substitute(to)),
           ...) {

    assert_no_intersect(from, to, name_from, name_to)
    turn <- node(mid_x, calc_pos(from$y0, from$y1, pos_from), r = 0)

    flowx(from, turn, pos = pos_from, arr_width = 0, name_from = name_from, ...)
    bend <- bendy(turn, to, label_from = label, pos_to = pos_to,
                  label_from_pos = label_pos, label_from_gap = label_gap,
                  label_from_x = label_x, label_from_y = label_y,
                  arr_width = arr_width, name_to = name_to, ...)

    list(x0 = min(from$x0, turn$x0, to$x0),
         y0 = min(from$y0, turn$y0, to$y0),
         x1 = max(from$x1, turn$x1, to$x1),
         y1 = max(from$y1, turn$y1, to$y1),
         x = turn$x, y = turn$y,
         from = from, turn1 = turn, turn2 = bend$turn, to = to)
  }


#' @title add turn between two nodes in the y-direction
#' @param from start node
#' @param to the end node
#' @param mid_y the y co-ordinate of the mid flow
#' @param label label to draw on the mid flow
#' @param pos_from a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = left and 1 = right
#' @param pos_to a decimal between 0 and 1 giving the position to attach the
#' flow to node `to`, where 0 = left and 1 = right
#' @param label_pos a decimal between 0 and 1 giving the position along the
#' `mid` flow to draw `label`, where 0 = left and 1 = right
#' @param label_gap distance from the `mid` flow at which to draw
#'  `label` default = 0.05
#'@param label_x x co-ordinate of `label` position, overrides use of `label_pos`
#'@param label_y y co-ordinate of `label` position, overrides use of `label_gap`
#' @param arr_width width of arrow, defaults to same as [flow()]
#' @param name_from internal argument used for informative error messages
#' @param name_to internal argument used for informative error messages
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export
turny <-
  function(from, mid_y, to, label = NULL,
           pos_from = NULL, pos_to = NULL,
           label_pos = NULL, label_gap = NULL, label_x = NULL, label_y = NULL,
           arr_width = NULL,
           name_from = deparse(substitute(from)),
           name_to = deparse(substitute(to)),
           ...) {

    assert_no_intersect(from, to, name_from, name_to)
    turn <- node(calc_pos(from$x0, from$x1, pos_from), mid_y, r = 0)

    flow(from, turn, pos = pos_from, arr_width = 0, name_from = name_from, ...)
    bend <- bendx(turn, to, label_from = label, pos_to = pos_to,
                  label_from_pos = label_pos, label_from_gap = label_gap,
                  label_from_x = label_x, label_from_y = label_y,
                  arr_width = arr_width, name_to = name_to, ...)

    list(x0 = min(from$x0, turn$x0, to$x0),
         y0 = min(from$y0, turn$y0, to$y0),
         x1 = max(from$x1, turn$x1, to$x1),
         y1 = max(from$y1, turn$y1, to$y1),
         x = turn$x, y = turn$y,
         from = from, turn1 = turn, turn2 = bend$turn, to = to)
  }
