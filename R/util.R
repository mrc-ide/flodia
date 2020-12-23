`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}

xoverlap1 <- function(node1, node2) {
  (node1$x0 >= node2$x0) & (node1$x0 <= node2$x1)
}

xoverlap <- function(node1, node2) {
  xoverlap1(node1, node2) | xoverlap1(node2, node1)
}

assert_xoverlap <- function(node1, node2,
                            name1 = deparse(substitute(node1)),
                            name2 = deparse(substitute(node2))) {
  if (!xoverlap(node1, node2)) {
    stop(sprintf("%s and %s must overlap in the x direction", name1, name2))
  }
}

yoverlap1 <- function(node1, node2) {
  (node1$y0 >= node2$y0) & (node1$y0 <= node2$y1)
}

yoverlap <- function(node1, node2) {
  yoverlap1(node1, node2) | yoverlap1(node2, node1)
}

assert_yoverlap <- function(node1, node2,
                            name1 = deparse(substitute(node1)),
                            name2 = deparse(substitute(node2))) {
  if (!yoverlap(node1, node2)) {
    stop(sprintf("%s and %s must overlap in the y direction", name1, name2))
  }
}
