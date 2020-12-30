#' @title add a node
#' @param x cental x position
#' @param y central y position
#' @param r radius of node
#' @param rx radius of node in x-direction
#' @param ry radius of node in y-direction
#' @param label node label
#' @param node_col colour of node
#' @param border_col colour of node border
#' @param label_font label font, see `par()`
#' @param label_col colour of `label`
#' @param label_cex size of `label` text
#' @param ... other formatting arguments to `rect()`
#' @importFrom graphics rect
#' @export
node <- function(x, y, r = 0.1,
                 label = "",
                 rx = r, ry = r,
                 node_col = "grey80", border_col = "black",
                 label_font = 1, label_col = "black", label_cex = 1, ...) {
  ret <- list(x = x, y = y, x0 = x - rx, y0 = y - ry, x1 = x + rx, y1 = y + ry)
  rect(ret$x0, ret$y0, ret$x1, ret$y1, col = node_col, border = border_col, ...)
  text(x, y, label, col = label_col, font = label_font, cex = label_cex)
  ret
}
