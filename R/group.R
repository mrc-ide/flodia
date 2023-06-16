#' @title group a set of nodes and flows
#' @param f flodia plot function
#' @param args list of named arguments to `f`
#' @param oma required border around nodes, vector of length 4 specifying:
#' bottom, left, top, right. defaults to 0.1 on all sides
#' @param group_col colour of group box
#' @param border_col colour of group border
#' @param label label to draw on group
#' @param label_pos_x a decimal between 0 and 1 giving the position on the group
#' to draw `label`, where 0 = left and 1 = right
#' @param label_pos_y a decimal between 0 and 1 giving the position on the group
#' to draw `label`, where 0 = bottom and 1 = top
#' @param label_x x co-ordinate of label position, overrides `label_pos_x`
#' @param label_y y co-ordinate of label position, overrides `label_pos_y`
#' @param label_col colour of `label`, defaults to black
#' @param label_font font of `label`, defaults to 1
#' @param label_cex label cex, defaults to 1
#' @param ... further arguments to `rect()` for formatting group
#' @return coordinates of group
#' @export
group <- function(f, args = list(), oma = rep(0.1, 4), group_col = NULL,
                  border_col = "black",
                  label = "", label_pos_x = 0.02, label_pos_y = 0.9,
                  label_font = 1, label_col = "black", label_cex = 1,
                  label_x = NULL, label_y = NULL, ...) {

  g <- invisible(do.call(f, args))

  g$x0 <- g$x0 - oma[2]
  g$y0 <- g$y0 - oma[1]
  g$x1 <- g$x1 + oma[4]
  g$y1 <- g$y1 + oma[3]
  g <- find_centre(g)

  label_x <- label_x %||% calc_pos(g$x0, g$x1, label_pos_x)
  label_y <- label_y %||% calc_pos(g$y0, g$y1, label_pos_y)

  rect(g$x0, g$y0, g$x1, g$y1, col = group_col, border = border_col, ...)
  text(x = label_x, y = label_y, labels = label, adj = 0, font = label_font,
       cex = label_cex, col = label_col)

  do.call(f, args)

  g
}
