test_that("flow works as expected", {
  si <- function(s_x = 0, s_y = 0, i_x = 1, i_y = 0) {
    r <- 0.1
    s <- node(s_x, s_y, r, label = "S")
    i <- node(i_x, i_y, r, label = "I")
    flow(s, i, label = expression(beta))
    list(x0 = s$x0, x1 = i$x1, y0 = s$y0, y1 = s$y1)
  }

  vdiffr::expect_doppelganger("SI diagram", fig = function() flodia(si))

  ##return error for non-overlapping nodes
  plot.new()
  expect_error(si(0, 0, 1, 1), "s and i must overlap in the x direction")
  expect_error(si(0, 0, 1, -1), "s and i must overlap in the x direction")
  expect_error(si(0, 0, -1, -1), "s and i must overlap in the x direction")
  expect_error(si(0, 0, -1, 1), "s and i must overlap in the y direction")
  dev.off()
})
