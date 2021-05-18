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
          label_to1 = "to1", arr_width0 = 0)
    forkx(mid, l0, l1, label_from = "nearer", label_from_pos = 0.8,
          label_to0 = "below", label_to0_gap = -0.05,
          label_to1 = "nearer", label_to1_pos = 0.2,
          arr_width1 = 0.2)
    forky(mid, t0, t1, label_from = "x", label_to0 = "y", label_to1 = "z",
          label_col = mid_palette("pu"), label_font = 2, arr_lty = 2,
          arr_width0 = 0)
    forky(mid, b0, b1, label_from = "x", label_to0 = "y", label_to1 = "z",
          arr_width1 = 0.2)

    list(x0 = l0$x0, x1 = r0$x1, y0 = b1$y0, y1 = t0$y1)
  }

  vdiffr::expect_doppelganger(title = "fork diagram",
                              fig = function() flodia(fork))

  # check error cases
  plot.new()
  r <- 0.1
  n1 <- node(0, 0, r, label = 1)
  n2 <- node(0, 1, r, label = 2)
  n3 <- node(1, 0, r, label = 3)
  expect_error(forkx(n1, n2, n3), "n1 and split must not intersect")
  expect_error(forky(n1, n2, n3), "n1 and split must not intersect")
  expect_error(forky(n1, n2, n2), "n2 and n2 must not intersect")
  expect_error(forkx(n1, n3, n3), "n3 and n3 must not intersect")
  dev.off()
})

test_that("can specify forkx and forky by length", {
  plot(0, 0, type = "n", xlim = c(-1, 2), ylim = c(-1, 2))
  r <- 0.1
  n2 <- node(0.7, 0.9, r, label = 2)
  n3 <- node(0.5, 0.1, r, label = 3)
  forkx(to0 = n2, to1 = n3, length = 0.5)
  forkx(to0 = n2, to1 = n3, length = -0.5)
  forky(to0 = n2, to1 = n3, length = 0.5)
  forky(to0 = n2, to1 = n3, length = -0.5)
  dev.off()
})
