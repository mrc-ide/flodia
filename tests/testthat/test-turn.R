test_that("turnx and turny work as expected", {
  sis <- function() {
    r <- 0.1
    s <- node(0, 0, r, label = "S[1]")
    i <- node(1, 0, r, label = "I[1]")
    s2 <- node(0, -1, r, label = "S[2]")
    i2 <- node(1, -1, r, label = "I[2]")
    flow(s, i, label = expression(beta))
    top <- i$y1 + r
    bottom <- i2$y0 - r
    left <- s$y0 -  r
    right <- i$x1 + r

    turny(i, top, s, label_mid = expression(gamma), arr_lty = 2)
    turny(i2, bottom, s2, label_mid = "under", label_mid_gap = -0.05,
          label_from = "low", label_to = "high",
          label_from_pos = 0.8, label_to_pos = 0.8,
          arr_col = mid_palette("bupu"), pos_from = 0.2, pos_to = 0.8)
    turnx(i, right, i2, pos_from = 0.2, pos_to = 0.8, label_mid = "inside",
          label_mid_gap = -0.1, label_from = "top", label_to = "bottom",
          label_to_gap = -0.05, label_to_pos = 0.2)
    turnx(s2, left, s, label_mid = "inside", label_mid_gap = 0.1, lwd = 2)
    list(x0 = left, x1 = right, y0 = bottom, y1 = top)
  }

  vdiffr::expect_doppelganger("SIS diagram", fig = function() flodia(sis))

  # check error cases
  plot.new()
  r <- 0.1
  n1 <- node(0, 0, r, label = 1)
  n2 <- node(0, 1, r, label = 2)
  n3 <- node(1, 0, r, label = 3)
  expect_error(turnx(n1, n1$x1 + 1, n1), "n1 and n1 must not intersect")
  expect_error(turnx(n1, n1$x, n2), "n1 and turn must not intersect")
  expect_error(turny(n1, n1$y1 + 1, n1), "n1 and n1 must not intersect")
  expect_error(turny(n1, n1$y, n3), "n1 and turn must not intersect")
  dev.off()
})
