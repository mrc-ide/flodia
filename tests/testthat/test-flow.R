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
  expect_error(si(0, 0, 1, 1),
              "s and i must overlap in either x or y direction")
  expect_error(si(0, 0, 0.1, 0.1), "s and i must not intersect")
  dev.off()
})

test_that("flowx works as expected", {
  six <- function(s_x = 0, s_y = 0, i_x = 1, i_y = 0) {
    r <- 0.1
    s <- node(s_x, s_y, r, label = "S")
    i <- node(i_x, i_y, r, label = "I")
    flowx(s, i, label = expression(beta))
    list(x0 = s$x0, x1 = i$x1, y0 = s$y0, y1 = s$y1)
  }

  ##return error for non-overlapping nodes
  plot.new()
  expect_error(six(0, 0, 1, 1),
               "s and i must overlap in the y direction")
  dev.off()
})

test_that("flowy works as expected", {
  siy <- function(s_x = 0, s_y = 0, i_x = 1, i_y = 0) {
    r <- 0.1
    s <- node(s_x, s_y, r, label = "S")
    i <- node(i_x, i_y, r, label = "I")
    flowy(s, i, label = expression(beta))
    list(x0 = s$x0, x1 = i$x1, y0 = s$y0, y1 = s$y1)
  }

  ##return error for non-overlapping nodes
  plot.new()
  expect_error(siy(0, 0, 1, 1),
               "s and i must overlap in the x direction")
  dev.off()
})

test_that("flowx and flowy work as expected", {
  inout <- function() {
    z <- node(0, 0, 0.5, label = "Z", label_cex = 3,
              label_col = mid_palette("gnbu"))

    finxlr <- flowx(to = z, length = 1, label = "inx L to R", pos = 0.2)
    finxrl <- flowx(to = z, length = -1, label = "inx R to L", pos = 0.2,
                    label_pos = 0.3, label_gap = -0.1,
                    label_col = mid_palette("bupu"),
                    arr_col = mid_palette("bu"),
                    arr_width = 0.2, arr_length = 0.2)

    finybt <- flowy(to = z, length = 1, label = "iny B to T", pos = 0.2,
                    label_gap = -0.2, label_pos = 0.8,
                    arr_col = mid_palette("gn"),
                    label_col = mid_palette("ylgn"))
    finytb <- flowy(to = z, length = -1, label = "iny T to B", pos = 0.2,
                    label_x = z$x0, label_y = z$y1 + 0.2)

    foutxlr <- flowx(from = z, length = 1, label = "outx L to R", pos = 0.8)
    foutxrl <- flowx(from = z, length = -1, label = "outx R to L", pos = 0.8,
                     label_pos = 0.3, label_gap = -0.1,
                     label_col = mid_palette("bupu"),
                     arr_col = mid_palette("bu"),
                     arr_width = 0.2, arr_length = 0.2)

    foutybt <- flowy(from = z, length = 1, label = "outy B to T", pos = 0.8,
                     label_gap = -0.2, label_pos = 0.2,
                     arr_col = mid_palette("gn"),
                     label_col = mid_palette("ylgn"))
    foutytb <- flowy(from = z, length = -1, label = "outy T to B", pos = 0.8,
                     label_x = z$x1, label_y = z$y0 - 0.2)


    list(x0 = finxlr$x0, x1 = finxrl$x1, y0 = finybt$y0, y1 = finytb$y1)
  }

  vdiffr::expect_doppelganger("in out flow diagram",
                              fig = function() flodia(inout))
})
