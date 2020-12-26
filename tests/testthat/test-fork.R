test_that("forkx and forky work as expected", {

  fork <- function() {
    r <- 0.1
    mid <- node(0, 0, r, label = "mid", node_col = light_palette("bupu"))
    r1  <- node(1, 0.4, r, label = "r1",  node_col = light_palette("ylgn"))
    r0  <- node(1, -0.4, r, label = "r0",  node_col = light_palette("ylgn"))

    l1  <- node(-1, 0.4, r, label = "l1",  node_col = light_palette("ylgn"))
    l0  <- node(-1, -0.4, r, label = "l0",  node_col = light_palette("ylgn"))

    t0 <- node(-0.4, 1, r, label = "t0", node_col = light_palette("gnbu"))
    t1 <- node(0.4, 0.8, r, label = "t1", node_col = light_palette("gnbu"))


    b0 <- node(-0.4, -0.8, r, label = "b0", node_col = light_palette("gnbu"))
    b1 <- node(0.4, -1, r, label = "b1", node_col = light_palette("gnbu"))

    forkx(mid, r0, r1, label_from = "from", label_to0 = "to0",
          label_to1 = "to1")
    forkx(mid, l0, l1, label_from = "nearer", label_from_pos = 0.8,
          label_to0 = "below", label_to0_gap = -0.05,
          label_to1 = "nearer", label_to1_pos = 0.2)
    forky(mid, t0, t1, label_from = "x", label_to0 = "y", label_to1 = "z",
          label_col = mid_palette("pu"), label_font = 2, arr_lty = 2)
    forky(mid, b0, b1, label_from = "x", label_to0 = "y", label_to1 = "z")

    list(x0 = l0$x0, x1 = r0$x1, y0 = b1$y0, y1 = t0$y1)
  }

  vdiffr::expect_doppelganger(title = "fork diagram",
                              fig = function() flodia(fork))
})
