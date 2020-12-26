test_that("bendx and bendy work as expected", {
  fivepoint <- function() {
    r <- 0.1
    mid <- node(0, 0, r, label = "mid", node_col = light_palette("bupu"))
    tl  <- node(-1, 1, r, label = "tl",  node_col = light_palette("ylgn"))
    tr  <- node(1, 1, r, label = "tr",  node_col = light_palette("gnbu"))
    br  <- node(1, -1, r, label = "br",  node_col = light_palette("gn"))
    bl  <- node(-1, -1, r, label = "bl",  node_col = light_palette("pugy"))


    ## standard

    bendy(mid, tr, label_from = "a", label_to = "b")
    bendx(mid, tr, label_from = "c", label_to = "d")

    ## adjust labels
    bendy(mid, tl, label_from = "left of flow", label_to = "below flow",
          pos_from = 0.2, label_from_gap = -0.2, label_to_gap = -0.05)
    bendx(mid, tl, label_from = "nearer from", label_from_pos = 0.3,
          label_to = "nearer to", label_to_pos = 0.7, label_to_gap = 0.15,
          pos_from = 0.7)


    ## adjust join points
    bendy(mid, br, label_from = "big", label_to = "wide arrow", label_cex = 1,
          label_from_gap = 0.1, arr_width = 0.3,
          pos_from = 0.8, pos_to = 0.9)
    bendx(mid, br, label_from = "long arrow", label_to = "", arr_length = 0.5,
          pos_from = 0.1, pos_to = 0.9)

    bendy(mid, bl, label_from = "purple", label_from_gap = -0.1,
          label_col = mid_palette("bupu"), label_to = "purple")
    bendx(mid, bl, label_from = "green bold", label_from_gap = -0.05,
          label_to = "green bold", label_to_gap = 0.2,
          label_col = mid_palette("gn"), label_font = 2)

    list(x0 = bl$x0, x1 = tr$x1, y0 = bl$y0, y1 = tr$y1)
  }


  vdiffr::expect_doppelganger(title = "bend diagram",
                              fig = function() flodia(fivepoint))
})
