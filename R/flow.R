
#' @title add a flow between two nodes
#' @param node0 start node
#' @param node1 end node
#' @param label label to draw on flow
#' @param pos a decimal between 0 and 1 giving the position to depart from the
#' start node, where 0 = bottom / left and 1 = top/right
#' @param label_pos a decimal between 0 and 1 giving the position along the flow
#' to draw the label, where 0 = bottom / left and 1 = top/right
#' @param label_gap distance from the flow at which to draw the label,
#' default = 0.1
#' @param label_col colour of label, defaults to black
#' @param font font of label, defaults to 3
#' @param label_cex label cex, defualts to 0.8
#' @param arr_type  type of arrow head to draw default = triangle
#' @param arr_length length of arrow head, defaults to 0.2
#' @param arr_width width of arrow head, defaults to 0.15
#' @param ... additional formatting arguments to arrow()
#' @return returns the start and end points of the flow
#' @importFrom shape Arrows
#' @importFrom graphics text
#' @export
flow <- function(node0 = NULL, node1 = NULL, label = "",
                 pos = 0.5, label_pos = 0.5,
                 label_gap = 0.1, label_col = "black", font = 3,
                 label_cex = 0.8,
                 arr_type = "triangle",
                 arr_length = 0.2, arr_width = 0.15,
                 ...) {

  x0 <- x1 <- calc_pos(node0$x0, node0$x1, pos)
  y0 <- y1 <- calc_pos(node0$y0, node0$y1, pos)

  if (node0$y0 > node1$y1) {
    y0 <- node0$y0
    y1 <- node1$y1
  }
  if (node0$x0 > node1$x1) {
    x0 <- node0$x0
    x1 <- node1$x1
  }
  if (node0$y1 < node1$y0) {
    y0 <- node0$y1
    y1 <- node1$y0
  }
  if (node0$x1 < node1$x0) {
    x0 <- node0$x1
    x1 <- node1$x0
  }

  Arrows(x0, y0, x1, y1, arr.type = arr_type, arr.adj = 1,
         arr.length = arr_length, arr.width = arr_width, ...)

  if (x0 == x1) {
    x <- x0 + label_gap
    y <- calc_pos(y0, y1, label_pos)
  } else if (y0 == y1) {
    x <- calc_pos(x0, x1, label_pos)
    y <- y0 + label_gap
  }

  text(x, y, labels = label, font = font, cex = label_cex, col = label_col)
  list(x = calc_pos(x0, x1), y = calc_pos(y0, y1),
       x0 = x0, y0 = y0, x1 = x1, y1 = y1)
}
