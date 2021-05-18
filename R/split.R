
#' @title add split between three nodes, in the x-direction
#' @inheritParams forkx
#' @param pos_to a decimal between 0 and 1 giving the position to attach the
#' flow to nodes `to0` and `to1`, where 0 = left and 1 = right
#' @return returns the start and end points of the flow
#' @export
splitx <-
  function(from = NULL, to0, to1, length = NULL,
           label_from = NULL, label_to0 = NULL, label_to1 = NULL,
           pos_from = NULL, pos_to = NULL,
           label_pos = NULL, label_from_pos = label_pos,
           label_to0_pos = NULL, label_to1_pos = NULL,
           label_gap = NULL, label_from_gap = label_gap,
           label_to0_gap = NULL, label_to1_gap = NULL,
           label_from_x = NULL, label_to0_x = NULL, label_to1_x = NULL,
           label_from_y = NULL, label_to0_y = NULL, label_to1_y = NULL,
           arr_width = NULL,
           name_from = deparse(substitute(from)),
           name_to0 = deparse(substitute(to0)),
           name_to1 = deparse(substitute(to1)), ...) {

    check_flow_args(to0, from, length)
    check_flow_args(to1, from, length)
    assert_xoverlap(to0, to1, name_to0, name_to1)
    assert_no_intersect(to0, to1, name_to0, name_to1)

    overlap <- xoverlap(to0, to1)
    if (length(from) == 0) { # specify based on length
      x <- ifelse(length > 0, overlap$x0, overlap$x1) - length
      from <- node(x, calc_pos(to0$y, to1$y, pos_from), r = 0)
    }

    split <- node(calc_pos(overlap$x0, overlap$x1, pos_to),
                  calc_pos(from$y0, from$y1, pos_from),
                  r = 0)

    flowx(from, split,
          label = label_from, label_pos = label_from_pos,
          label_gap = label_from_gap, label_x = label_from_x,
          label_y = label_from_y, arr_width = 0,
          name_from = name_from, ...)
    flowy(split, to0,
          label = label_to0, label_pos = label_to0_pos,
          label_gap = label_to0_gap, label_x = label_to0_x,
          label_y = label_to0_y, arr_width = arr_width,
          name_to = name_to0, ...)
    flowy(split, to1,
          label = label_to1, label_pos = label_to1_pos,
          label_gap = label_to1_gap, label_x = label_to1_x,
          label_y = label_to1_y, arr_width = arr_width,
          name_to = name_to1, ...)

    list(x0 = min(from$x0, to0$x0, to1$x0),
         y0 = min(from$y0, to0$y0, to1$y0),
         x1 = max(from$x1, to0$x1, to1$x1),
         y1 = max(from$y1, to0$y1, to1$y1),
         x = split$x, y = split$y,
         from = from, split = split, to0 = to0, to1 = to1)
  }


#' @title add split between three nodes, in the y-direction
#' @inheritParams forky
#' @param pos_to a decimal between 0 and 1 giving the position to attach the
#' flow to nodes `to0` and `to1`, where 0 = bottom and 1 = top
#' @return returns the start and end points of the flow
#' @export
splity <-
  function(from = NULL, to0, to1, length = NULL,
           label_from = NULL, label_to0 = NULL, label_to1 = NULL,
           pos_from = NULL, pos_to = NULL,
           label_pos = NULL, label_from_pos = label_pos,
           label_to0_pos = NULL, label_to1_pos = NULL,
           label_gap = NULL, label_from_gap = label_gap,
           label_to0_gap = NULL, label_to1_gap = NULL,
           label_from_x = NULL, label_to0_x = NULL, label_to1_x = NULL,
           label_from_y = NULL, label_to0_y = NULL, label_to1_y = NULL,
           arr_width = NULL,
           name_from = deparse(substitute(from)),
           name_to0 = deparse(substitute(to0)),
           name_to1 = deparse(substitute(to1)), ...) {

    check_flow_args(to0, from, length)
    check_flow_args(to1, from, length)
    assert_yoverlap(to0, to1, name_to0, name_to1)
    assert_no_intersect(to0, to1, name_to0, name_to1)

    overlap <- yoverlap(to0, to1)
    if (length(from) == 0) { # specify based on length
      y <- ifelse(length > 0, overlap$y0, overlap$y1) - length
      from <- node(calc_pos(to0$x, to1$x, pos_from), y, r = 0)
    }

    split <- node(calc_pos(from$x0, from$x1, pos_from),
                  calc_pos(overlap$y0, overlap$y1, pos_to),
                  r = 0)

    flowy(from, split,
          label = label_from, label_pos = label_from_pos,
          label_gap = label_from_gap, label_x = label_from_x,
          label_y = label_from_y, arr_width = 0,
          name_from = name_from, ...)
    flowx(split, to0,
          label = label_to0, label_pos = label_to0_pos,
          label_gap = label_to0_gap, label_x = label_to0_x,
          label_y = label_to0_y, arr_width = arr_width,
          name_to = name_to0, ...)
    flowx(split, to1,
          label = label_to1, label_pos = label_to1_pos,
          label_gap = label_to1_gap, label_x = label_to1_x,
          label_y = label_to1_y, arr_width = arr_width,
          name_to = name_to1, ...)

    list(x0 = min(from$x0, to0$x0, to1$x0),
         y0 = min(from$y0, to0$y0, to1$y0),
         x1 = max(from$x1, to0$x1, to1$x1),
         y1 = max(from$y1, to0$y1, to1$y1),
         x = split$x, y = split$y,
         from = from, split = split, to0 = to0, to1 = to1)
  }
