#' @title add a flow between two nodes
#' @param from start node
#' @param to end node
#' @param label label to draw on flow
#' @param pos a decimal between 0 and 1 giving the position of the flow relative
#' to the nodes. Where 0 = bottom/left and 1 = top/right of the
#' interval over which the nodes overlap.
#' @param label_pos a decimal between 0 and 1 giving the position along the flow
#' to draw `label`, where 0 = bottom / left and 1 = top/right
#' @param label_gap distance from the flow at which to draw the label,
#' default = 0.1
#' @param label_x x co-ordinate of label position, overrides use of `label_pos`
#' and / or `label_gap`
#' @param label_y y co-ordinate of label position, overrides use of `label_pos`
#' and / or `label_gap`
#' @param label_col colour of `label`, defaults to black
#' @param label_font font of `label`, defaults to 3
#' @param label_cex label cex, defaults to 0.8
#' @param arr_type  type of arrow head to draw default = triangle
#' @param arr_col colour of arrow default = black
#' @param arr_length length of arrow head, defaults to 0.2
#' @param arr_width width of arrow head, defaults to 0.1
#' @param arr_lty lty of arrow, defaults to 1
#' @param name_from internal argument used for informative error messages
#' @param name_to internal argument used for informative error messages
#' @param ... additional formatting arguments to arrow()
#' @return returns the start and end points of the flow
#' @importFrom shape Arrowhead
#' @importFrom graphics text
#' @importFrom graphics segments
#' @export
flow <- function(from, to, label = NULL,
                 pos = NULL, label_pos = NULL,
                 label_gap = NULL, label_x = NULL, label_y = NULL,
                 label_col = "black", label_font = 3,
                 label_cex = 0.8,
                 arr_type = "triangle", arr_col = "black",
                 arr_length = 0.15, arr_width = NULL, arr_lty = 1,
                 name_from = deparse(substitute(from)),
                 name_to = deparse(substitute(to)),
                 ...) {

  if (length(yoverlap(from, to)) > 1) { # flow in x-direction
    flowx(from, to, length = NULL,
          label, pos, label_pos, label_gap, label_x, label_y, label_col,
          label_font, label_cex,
          arr_type, arr_col, arr_length, arr_width, arr_lty,
          name_from, name_to, ...)
  } else if (length(xoverlap(from, to)) > 1) { # flow in y-direction
    flowy(from, to, length = NULL,
          label, pos, label_pos, label_gap, label_x, label_y, label_col,
          label_font, label_cex,
          arr_type, arr_col, arr_length, arr_width, arr_lty,
          name_from, name_to, ...)
  } else {
    stop(sprintf("%s and %s must overlap in either x or y direction",
                 name_from, name_to))
  }
}

#' @title add a flow between two nodes in the x-direction
#' @param from start node
#' @param to end node
#' @param length single numeric specifying length, positive will result in a
#' flow from left to right, negative from right to left. Optional parameter
#' replacing either `to` or `from`,
#' @param label label to draw on flow
#' @param pos a decimal between 0 and 1 giving the position of the flow relative
#' to the nodes. Where 0 = bottom and 1 = top of the
#' interval over which the nodes overlap.
#' @param label_pos a decimal between 0 and 1 giving the x-position along the
#' flow to draw the label, where 0 = left and 1 = right
#' @param label_gap y-distance from the flow at which to draw the label,
#' default = 0.05
#' @param label_x x co-ordinate of label position, overrides use of `label_pos`
#' @param label_y y co-ordinate of label position, overrides use of `label_gap`
#' @param label_col colour of label, defaults to black
#' @param label_font font of label, defaults to 3
#' @param label_cex label cex, defaults to 0.8
#' @param arr_type  type of arrow head to draw default = triangle
#' @param arr_col colour of arrow default = black
#' @param arr_length length of arrow head, defaults to 0.2
#' @param arr_width width of arrow head, defaults to 0.1
#' @param arr_lty lty of arrow, defaults to 1
#' @param name_from internal argument used for informative error messages
#' @param name_to internal argument used for informative error messages
#' @param ... additional formatting arguments to arrow()
#' @return returns the start and end points of the flow
#' @importFrom shape Arrowhead
#' @importFrom graphics text
#' @importFrom graphics segments
#' @export
flowx <- function(from = NULL, to = NULL, length = NULL, label = NULL,
                  pos = NULL, label_pos = NULL,
                  label_gap = NULL, label_x = NULL, label_y = NULL,
                  label_col = "black", label_font = 3,
                  label_cex = 0.8,
                  arr_type = "triangle", arr_col = "black",
                  arr_length = 0.15, arr_width = NULL, arr_lty = 1,
                  name_from = deparse(substitute(from)),
                  name_to = deparse(substitute(to)),
                  ...) {

  label <- label %||% ""
  pos <- pos %||% 0.5
  label_pos <- label_pos %||% 0.5
  label_gap <- label_gap %||% 0.05
  arr_width <- arr_width %||% 0.1

  check_flow_args(to, from, length)

  if (length(from) == 0) { # inflow
    x <- ifelse(length > 0, to$x0, to$x1) - length
    from <- node(x, calc_pos(to$y0, to$y1, pos), r = 0)
  } else if (length(to) == 0) { # outflow
    x <- ifelse(length > 0, from$x1, from$x0) + length
    to <- node(x, calc_pos(from$y0, from$y1, pos), r = 0)
  }

  assert_no_intersect(from, to, name_from, name_to)
  assert_yoverlap(from, to, name_from, name_to)

  overlap <- yoverlap(from, to)
  y <- calc_pos(overlap$y0, overlap$y1, pos)
  label_y <- label_y %||% (y + label_gap)

  if (from$x0 > to$x1) { # left
    from_x <- from$x0
    to_x <- to$x1
  } else if (from$x1 < to$x0) { # right
    from_x <- from$x1
    to_x <- to$x0
  }

  label_x <- label_x %||% calc_pos(from_x, to_x, label_pos)

  add_arrow(from_x, to_x, y, y,
            label, label_x, label_y, label_font, label_cex, label_col,
            arr_width, arr_type, arr_length, arr_col, arr_lty, ...)

  list(x = calc_pos(from_x, to_x),
       y = y,
       x0 = min(from_x, to_x),
       y0 = y,
       x1 = max(from_x, to_x),
       y1 = y)
}


#' @title add a flow between two nodes in the y-direction
#' @param from start node
#' @param to end node
#' @param length single numeric specifying length, positive will result in a
#' flow from left to right, negative from bottom to top. Optional parameter
#' replacing either `to` or `from`,
#' @param label label to draw on flow
#' @param pos a decimal between 0 and 1 giving the position of the flow relative
#' to the nodes. Where 0 = left and 1 = right of the interval over which the
#' nodes overlap.
#' @param label_pos a decimal between 0 and 1 giving the y-position along the
#' flow to draw the label, where 0 = bottom and 1 = top
#' @param label_gap x-distance from the flow at which to draw the label,
#' default = 0.05
#' @param label_x x co-ordinate of label position, overrides use of `label_gap`
#' @param label_y y co-ordinate of label position, overrides use of `label_pos`
#' @param label_col colour of label, defaults to black
#' @param label_font font of label, defaults to 3
#' @param label_cex label cex, defaults to 0.8
#' @param arr_type type of arrow head to draw default = triangle
#' @param arr_col colour of arrow default = black
#' @param arr_length length of arrow head, defaults to 0.2
#' @param arr_width width of arrow head, defaults to 0.1
#' @param arr_lty lty of arrow, defaults to 1
#' @param name_from internal argument used for informative error messages
#' @param name_to internal argument used for informative error messages
#' @param ... additional formatting arguments to arrow()
#' @return returns the start and end points of the flow
#' @importFrom shape Arrowhead
#' @importFrom graphics text
#' @importFrom graphics segments
#' @export
flowy <- function(from = NULL, to = NULL, length = NULL, label = NULL,
                  pos = NULL, label_pos = NULL,
                  label_gap = NULL, label_x = NULL, label_y = NULL,
                  label_col = "black", label_font = 3,
                  label_cex = 0.8,
                  arr_type = "triangle", arr_col = "black",
                  arr_length = 0.15, arr_width = NULL, arr_lty = 1,
                  name_from = deparse(substitute(from)),
                  name_to = deparse(substitute(to)),
                  ...) {

  label <- label %||% ""
  pos <- pos %||% 0.5
  label_pos <- label_pos %||% 0.5
  label_gap <- label_gap %||% 0.05
  arr_width <- arr_width %||% 0.1

  check_flow_args(to, from, length)

  if (length(from) == 0) { # inflow
    y <- ifelse(length > 0, to$y0, to$y1) - length
    from <- node(calc_pos(to$x0, to$x1, pos), y, r = 0)
  } else if (length(to) == 0) { # outflow
    y <- ifelse(length > 0, from$y1, from$y0) + length
    to <- node(calc_pos(from$x0, from$x1, pos), y, r = 0)
  }

  assert_no_intersect(from, to, name_from, name_to)
  assert_xoverlap(from, to, name_from, name_to)

  overlap <- xoverlap(from, to)
  x <- calc_pos(overlap$x0, overlap$x1, pos)
  label_x <- label_x %||% (x + label_gap)

  if (from$y0 > to$y1) { # down
    from_y <- from$y0
    to_y <- to$y1
  } else if (from$y1 < to$y0) { # up
    from_y <- from$y1
    to_y <- to$y0
  }

  label_y <- label_y %||% calc_pos(from_y, to_y, label_pos)

  add_arrow(x, x, from_y, to_y,
            label, label_x, label_y, label_font, label_cex, label_col,
            arr_width, arr_type, arr_length, arr_col, arr_lty, ...)

  list(x = x,
       y = calc_pos(from_y, to_y),
       x0 = x,
       y0 = min(from_y, to_y),
       x1 = x,
       y1 = max(from_y, to_y))
}


add_arrow <- function(from_x, to_x, from_y, to_y,
                      label, label_x, label_y, label_font, label_cex, label_col,
                      arr_width, arr_type, arr_length, arr_col, arr_lty, ...) {

  segments(from_x, from_y, to_x, to_y, lty = arr_lty, col = arr_col, ...)

  if (arr_width > 0) {
    angle <- atan((to_y - from_y) / (to_x - from_x)) / pi * 180
    angle[is.nan(angle)] <- 0
    angle[to_x < from_x] <- 180 + angle[to_x < from_x]
    Arrowhead(to_x, to_y, angle = angle, arr.type = arr_type, arr.adj = 1,
              arr.lwd = 1e-6, arr.length = arr_length, arr.width = arr_width,
              lty = 1, arr.col = arr_col, lcol = arr_col, ...)
  }

  text(label_x, label_y, labels = label, font = label_font, cex = label_cex,
       col = label_col, adj = 0.5)
}
