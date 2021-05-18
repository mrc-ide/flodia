test_that("splitx and splity work as expected", {
  split <- function() {
    r <- 0.1
    mid <- node(0, 0, r, label = "mid", node_col = light_palette("bupu"))
    r1  <- node(0.8, 0.4, r * 2, label = "r1", node_col = light_palette("ylgn"))
    r0  <- node(1, -0.4, r, label = "r0",  node_col = light_palette("ylgn"))

    l1  <- node(-1, 0.4, r, label = "l1",  node_col = light_palette("ylgn"))
    l0  <- node(-1, -0.4, r, label = "l0",  node_col = light_palette("ylgn"))

    t0 <- node(-0.4, 1, r, label = "t0", node_col = light_palette("gnbu"))
    t1 <- node(0.4, 1, r, label = "t1", node_col = light_palette("gnbu"))

    b0 <- node(-0.4, -1, r, label = "b0", node_col = light_palette("gnbu"))
    b1 <- node(0.4, -1, r, label = "b1", node_col = light_palette("gnbu"))

    splitx(mid, r0, r1, label_from = "from", label_to0 = "to0",
           label_to1 = "to1", arr_width0 = 0)
    splitx(mid, l0, l1,
           pos_from = 0.3, pos_to = 0.3,
           label_from = "nearer", label_from_pos = 0.2,
           label_to0 = "left", label_to0_gap = -0.05,
           label_to1 = "low", label_to1_pos = 0.2, arr_width1 = 0.2)
    splity(mid, t0, t1, label_from = "x", label_to0 = "y", label_to1 = "z",
           label_col = mid_palette("pu"), label_font = 2, arr_lty = 2,
           arr_width0 = 0)
    splity(mid, b0, b1,
           pos_from = 0.3, pos_to = 0.3,
           label_from = "x", label_to0 = "y", label_to1 = "z",
           arr_col = mid_palette("bupu"), arr_width1 = 0.2)

    list(x0 = l0$x0, x1 = r0$x1, y0 = b1$y0, y1 = t0$y1)
  }


  vdiffr::expect_doppelganger(title = "split diagram",
                              fig = function() flodia(split))

  # check error cases
  plot.new()
  r <- 0.1
  n1 <- node(0, 0, r, label = 1)
  n2 <- node(0, 1, r, label = 2)
  n3 <- node(1, 0, r, label = 3)
  n4 <- node(1, 1, r, label = 4)
  expect_error(splitx(n1, n2, n3), "n2 and n3 must overlap in the x direction")
  expect_error(splity(n1, n2, n3), "n2 and n3 must overlap in the y direction")
  expect_error(splitx(n1, n3, n4), "split and n3 must not intersect")
  expect_error(splitx(n2, n3, n4), "split and n4 must not intersect")
  expect_error(splity(n1, n2, n4), "split and n2 must not intersect")
  expect_error(splity(n2, n3, n1), "split and n1 must not intersect")
  expect_error(splity(n1, n2, n2), "n2 and n2 must not intersect")
  expect_error(splitx(n1, n3, n3), "n3 and n3 must not intersect")
  dev.off()

})
