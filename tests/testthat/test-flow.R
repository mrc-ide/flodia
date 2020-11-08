context("flows")

  si <- function() {
    r <- 0.1
    s <- node(0, 0, r, label = "S")
    i <- node(1, 0, r, label = "I")
    flow(s, i, label = expression(beta))
    list(x0 = s$x0, x1 = i$x1, y0 = s$y0, y1 = s$y1)
  }

  vdiffr::expect_doppelganger("SI diagram", fig = function() flodia(si))
