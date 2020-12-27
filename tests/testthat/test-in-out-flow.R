test_that("inflow and outflow work as expected", {
  inout <- function() {
    z <- node(0, 0, 0.5, label = "Z", label_cex = 3,
              label_col = mid_palette("gnbu"))

    finxlr <- inflowx(z, 1, label = "inx L to R", pos = 0.2)
    finxrl <- inflowx(z, -1, label = "inx R to L", pos = 0.2,
                      label_pos = 0.3, label_gap = -0.1,
                      label_col = mid_palette("bupu"),
                      arr_col = mid_palette("bu"),
                      arr_width = 0.2, arr_length = 0.2)

    finybt <- inflowy(z, 1, label = "iny B to T", pos = 0.2,
                      label_gap = -0.2, label_pos = 0.8,
                      arr_col = mid_palette("gn"),
                      label_col = mid_palette("ylgn"))
    finytb <- inflowy(z, -1, label = "iny T to B", pos = 0.2,
                      label_x = z$x0, label_y = z$y1 + 0.2)

    foutxlr <- outflowx(z, 1, label = "outx L to R", pos = 0.8)
    foutxrl <- outflowx(z, -1, label = "outx R to L", pos = 0.8,
                        label_pos = 0.3, label_gap = -0.1,
                        label_col = mid_palette("bupu"),
                        arr_col = mid_palette("bu"),
                        arr_width = 0.2, arr_length = 0.2)

    foutybt <- outflowy(z, 1, label = "outy B to T", pos = 0.8,
                        label_gap = -0.2, label_pos = 0.2,
                        arr_col = mid_palette("gn"),
                        label_col = mid_palette("ylgn"))
    foutytb <- outflowy(z, -1, label = "outy T to B", pos = 0.8,
                        label_x = z$x1, label_y = z$y0 - 0.2)


    list(x0 = finxlr$x0, x1 = finxrl$x1, y0 = finybt$y0, y1 = finytb$y1)
  }

  vdiffr::expect_doppelganger("in-out-flow-diagram",
                              fig = function() flodia(inout))
})
