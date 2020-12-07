#' @title group a set of nodes and flows
#' @param x0 left-most x
#' @param y0 bottom-most y
#' @param x1 right-most x
#' @param y1 top-most y
#' @param gap required border around nodes, vector of length 4 specifying:
#' bottom, left, top, right. defaults to 0.1 on all sides
#' @param col colour of group box
#' @param border colour of group border
#' @return coordinates of group
#' @export
group <- function(x0, y0, x1, y1, gap = rep(0.1, 4), col = "grey90",
                  border = "black") {
  ret <- list(x0 = x0 - gap[2], y0 = y0 - gap[1],
              x1 = x1 + gap[4], y1 = y1 + gap[3])
  ret$x <- calc_pos(ret$x0, ret$x1)
  ret$y <- calc_pos(ret$y0, ret$y1)
  rect(ret$x0, ret$y0, ret$x1, ret$y1, col = col, border = border)
  ret
}
