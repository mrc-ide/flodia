#' @title add a node
#' @param x cental x position
#' @param y central y position
#' @param r radius of node
#' @param label node label
#' @param x_root shift from x = 0 (default = 0)
#' @param y_root shift from y = 0 (default = 0)
#' @param node_col colour of node
#' @param border colour of node border
#' @param font label font, see par()
#' @param label_col colour of
#' @importFrom graphics rect
#' @export
node <- function(x, y, r, label = "", x_root = 0, y_root = 0,
                 node_col = "grey80", border = "black",
                 font = 1, label_col = "black") {
  x <- x + x_root
  y <- y + y_root
  ret <- list(x = x, y = y, x0 = x - r, y0 = y - r, x1 = x + r, y1 = y + r)
  rect(ret$x0, ret$y0, ret$x1, ret$y1, col = node_col, border = border)
  text(x, y, label, col = label_col, font = font)
  ret
}
