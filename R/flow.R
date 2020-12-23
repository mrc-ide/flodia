
#' @title add a flow between two nodes
#' @param from start node
#' @param to end node
#' @param label label to draw on flow
#' @param pos a decimal between 0 and 1 giving the position of the flow relative
#' to the nodes. Where 0 = bottom/left and 1 = top/right of the
#' interval over which the nodes overlap.
#' @param label_pos a decimal between 0 and 1 giving the position along the flow
#' to draw the label, where 0 = bottom / left and 1 = top/right
#' @param label_gap distance from the flow at which to draw the label,
#' default = 0.1
#' @param label_col colour of label, defaults to black
#' @param label_font font of label, defaults to 3
#' @param label_cex label cex, defaults to 0.8
#' @param arr_type  type of arrow head to draw default = triangle
#' @param arr_col colour of arrow default = black
#' @param arr_length length of arrow head, defaults to 0.2
#' @param arr_width width of arrow head, defaults to 0.1
#' @param arr_lty lty of arrow, defaults to 1
#' @param ... additional formatting arguments to arrow()
#' @return returns the start and end points of the flow
#' @importFrom shape Arrowhead
#' @importFrom graphics text
#' @importFrom graphics segments
#' @export
flow <- function(from, to, label = NULL,
                 pos = NULL, label_pos = NULL,
                 label_gap = NULL, label_col = "black", label_font = 3,
                 label_cex = 0.8,
                 arr_type = "triangle", arr_col = "black",
                 arr_length = 0.15, arr_width = NULL, arr_lty = 1,
                 ...) {

  label <- label %||% ""
  pos <- pos %||% 0.5
  label_pos <- label_pos %||% 0.5
  label_gap <- label_gap %||% 0.05
  arr_width <- arr_width %||% 0.1

  name_from <- deparse(substitute(from))
  name_to  <- deparse(substitute(to))

  if (from$y0 > to$y1) { # down
    assert_xoverlap(from, to, name_from, name_to)
    y0 <- from$y0
    y1 <- to$y1
    overlap <- xoverlap(from, to)
    x0 <- x1 <- calc_pos(overlap$x0, overlap$x1, pos)
    x <- x0 + label_gap
    y <- calc_pos(from$y0, to$y1, label_pos)
  } else if (from$x0 > to$x1) { # left
    assert_yoverlap(from, to, name_from, name_to)
    x0 <- from$x0
    x1 <- to$x1
    overlap <- yoverlap(from, to)
    y0 <- y1 <- calc_pos(overlap$y0, overlap$y1, pos)
    x <- calc_pos(from$x0, to$x1, label_pos)
    y <- y0 + label_gap
  } else if (from$y1 < to$y0) { # up
    assert_xoverlap(from, to, name_from, name_to)
    y0 <- from$y1
    y1 <- to$y0
    overlap <- xoverlap(from, to)
    x0 <- x1 <- calc_pos(overlap$x0, overlap$x1, pos)
    x <- x0 + label_gap
    y <- calc_pos(from$y1, to$y0, label_pos)
  } else if (from$x1 < to$x0) { # right
    assert_yoverlap(from, to, name_from, name_to)
    x0 <- from$x1
    x1 <- to$x0
    overlap <- yoverlap(from, to)
    y0 <- y1 <- calc_pos(overlap$y0, overlap$y1, pos)
    x <- calc_pos(from$x1, to$x0, label_pos)
    y <- y0 + label_gap
  }

  segments(x0, y0, x1, y1, lty = arr_lty, col = arr_col, ...)

  if (arr_width > 0) {
    angle <- atan((y1 - y0) / (x1 - x0)) / pi * 180
    angle[is.nan(angle)] <- 0
    angle[x1 < x0] <- 180 + angle[x1 < x0]
    Arrowhead(x1, y1, angle = angle, arr.type = arr_type, arr.adj = 1,
              arr.lwd = 1e-6, arr.length = arr_length, arr.width = arr_width,
              lty = 1, arr.col = arr_col, lcol = arr_col, ...)
  }

  text(x, y, labels = label, font = label_font, cex = label_cex,
       col = label_col)
  list(x = calc_pos(x0, x1), y = calc_pos(y0, y1),
       x0 = x0, y0 = y0, x1 = x1, y1 = y1)
}
