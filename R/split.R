
#' @title add split between three nodes, in direction x
#' @param from start node
#' @param to0 the bottom end node
#' @param to1 the top end node
#' @param label_from label to draw on the flow out of `from`
#' @param label_to0 label to draw on the flow into `to0`
#' @param label_to1 label to draw on the flow into `to1`
#' @param pos_from a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = bottom and 1 = top
#' @param pos_to a decimal between 0 and 1 giving the position to attach the
#' flow to node `to0` and `to1`, where 0 = bottom and 1 = top
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
#' @param arr_width width of arrow, defaults to same as [flow()]
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export
splitx <-
  function(from, to0, to1,
           label_from = NULL, label_to0 = NULL, label_to1 = NULL,
           pos_from = NULL, pos_to = NULL,
           label_from_pos = NULL, label_to0_pos = NULL, label_to1_pos = NULL,
           label_from_gap = NULL, label_to0_gap = NULL, label_to1_gap = NULL,
           arr_width = NULL, ...) {

    bendx(from, to0, pos_from = pos_from, pos_to = pos_to,
          label_from = label_from, label_from_pos = label_from_pos,
          label_from_gap = label_from_gap,
          label_to = label_to0, label_to_pos = label_to0_pos,
          label_to_gap = label_to0_gap, arr_width = arr_width, ...)
    bendx(from, to1, pos_from = pos_from, pos_to = pos_to,
          label_to = label_to1, label_to_pos = label_to1_pos,
          label_to_gap = label_to1_gap, arr_width = arr_width, ...)

    list(x0 = min(from$x0, to0$x0, to1$x0),
         y0 = min(from$y0, to0$y0, to1$y0),
         x1 = max(from$x1, to0$x1, to1$x1),
         y1 = max(from$y1, to0$y1, to1$y1),
         x = from$x, y = from$y)
  }


#' @title add split between three nodes, in direction y
#' @param from start node
#' @param to0 the left end node
#' @param to1 the right end node
#' @param label_from label to draw on the flow out of `from`
#' @param label_to0 label to draw on the flow into `to0`
#' @param label_to1 label to draw on the flow into `to1`
#' @param pos_from a decimal between 0 and 1 giving the position to start the
#'  flow out of `from`, where 0 = left and 1 = right
#' @param pos_to a decimal between 0 and 1 giving the position to attach the
#' flow to nodes `to0` and `to1`, where 0 = left and 1 = right
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
#' @param arr_width width of arrow, defaults to same as [flow()]
#' @param ... additional formatting arguments to [flow()]
#' @return returns the start and end points of the flow
#' @export
splity <-
  function(from, to0, to1,
           label_from = NULL, label_to0 = NULL, label_to1 = NULL,
           pos_from = NULL, pos_to = NULL,
           label_from_pos = NULL, label_to0_pos = NULL, label_to1_pos = NULL,
           label_from_gap = NULL, label_to0_gap = NULL, label_to1_gap = NULL,
           arr_width = NULL, ...) {

    bendy(from, to0, pos_from = pos_from, pos_to = pos_to,
          label_from = label_from, label_from_pos = label_from_pos,
          label_from_gap = label_from_gap,
          label_to = label_to0, label_to_pos = label_to0_pos,
          label_to_gap = label_to0_gap, arr_width = arr_width, ...)
    bendy(from, to1, pos_from = pos_from, pos_to = pos_to,
          label_to = label_to1, label_to_pos = label_to1_pos,
          label_to_gap = label_to1_gap, arr_width = arr_width, ...)

    list(x0 = min(from$x0, to0$x0, to1$x0),
         y0 = min(from$y0, to0$y0, to1$y0),
         x1 = max(from$x1, to0$x1, to1$x1),
         y1 = max(from$y1, to0$y1, to1$y1),
         x = from$x, y = from$y)
  }
