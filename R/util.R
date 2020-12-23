`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}

xoverlap1 <- function(node0, node1) {
  if ((node1$x0 >= node0$x0) & (node1$x0 <= node0$x1)) {
    ret <- list(x0 = node1$x0, x1 = min(node0$x1, node1$x1))
  } else {
    ret <- NA
  }

  ret
}

xoverlap <- function(node0, node1) {
  overlap <- xoverlap1(node0, node1)
  if (length(overlap) == 1) {
    overlap <- xoverlap1(node1, node0)
  }
  overlap
}

assert_xoverlap <- function(node0, node1,
                            name0 = deparse(substitute(node0)),
                            name1 = deparse(substitute(node1))) {
  if (length(xoverlap(node0, node1)) == 1) {
    stop(sprintf("%s and %s must overlap in the x direction", name0, name1))
  }
}

yoverlap1 <- function(node0, node1) {
  if ((node1$y0 >= node0$y0) & (node1$y0 <= node0$y1)) {
    ret <- list(y0 = node1$y0, y1 = min(node0$y1, node1$y1))
  } else {
    ret <- NA
  }

  ret
}

yoverlap <- function(node0, node1) {

  overlap <- yoverlap1(node0, node1)
  if (length(overlap) == 1) {
    overlap <- yoverlap1(node1, node0)
  }
  overlap
}

assert_yoverlap <- function(node0, node1,
                            name0 = deparse(substitute(node0)),
                            name1 = deparse(substitute(node1))) {
  if (length(yoverlap(node0, node1)) == 1) {
    stop(sprintf("%s and %s must overlap in the y direction", name0, name1))
  }
}
