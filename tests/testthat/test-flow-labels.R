test_that("can specify exact x and y co-ordinates of flow labels", {
  all_flows <- function() {
    r <- 0.1
    xgap <- ygap <- 0.4
    a <- node(1, 3, r, "A")
    b <- node(a$x - xgap / 2, a$y - 2 * ygap, r, "B")
    c <- node(a$x + xgap / 2, b$y, r, "C")
    d <- node(a$x + 2 * xgap, a$y, r, "D")
    e <- node(d$x, c$y - ygap, r, "E")
    f <- node(d$x + 2 * xgap, d$y + ygap / 2, r, "F")
    g <- node(f$x, f$y - ygap, r, "G")
    h <- node(g$x + xgap, e$y, r, "H")

    lx0 <- b$x0 - xgap / 3
    lx1 <- calc_pos(a$x1, d$x0, 0.7)
    lx2 <- calc_pos(d$x1, f$x0, 0.3)
    lx3 <- calc_pos(d$x1, f$x0, 0.7)
    lx4 <- g$x + 0.08

    ly1 <- calc_pos(b$y1, a$y0, 0.7)
    ly2 <- calc_pos(b$y1, a$y0, 0.3)
    ly3 <- calc_pos(e$y1, b$y0, 0.6)

    ad <- flow(a, d, "ad", label_x = lx1)
    fg <- flow(f, g, "fg", label_x = lx4)
    eb <- bendx(e, b, pos_from = 0.3, label_from = "eb1", label_from_x = lx1,
                label_from_gap = -0.05,
                label_to = "eb2", label_to_y = ly3, label_to_gap = 0.07)
    ce <- bendy(c, e, pos_to = 0.7, label_to = "ce2", label_to_x = lx1,
                label_from = "ce1", label_from_y = ly3, label_from_gap = 0.07)
    dgh <- forkx(d, g, f, label_from = "dgf", label_from_x = lx2,
                 label_to0 = "dg", label_to0_x = lx3,
                 label_to1 = "df")
    abc <- forky(a, b, c, label_from = "abc", label_to0 = "ab",
                 label_to1 = "ac", label_from_y = ly1, label_from_gap = 0.1,
                 label_to0_y = ly2, label_to0_gap = 0.07
    )
    ced <- splitx(c, e, d, label_from = "ced", label_from_x = lx1,
                  label_to1 = "cd", label_to1_y = ly2, label_to1_gap = 0.07,
                  label_to0 = "ce", label_to0_y = ly3, label_to0_gap = 0.07)
    geh <- splity(g, e, h, label_from = "geh",
                  label_from_y = ly2, label_from_x = lx4,
                  label_to0 = "ge", label_to0_x = lx3,
                  label_to1 = "gh", label_to1_x = lx4)
    ba <- turnx(b, lx0 - 0.1, a, label = "ba",
                label_x = lx0, label_y = ly1)
    da <- turny(d, f$y, a, label = "da", label_x = lx1)

    list(x0 = ba$x0, x1 = h$x1, y0 = e$y0, y1 = f$y1)
  }

  vdiffr::expect_doppelganger("flow label diagram",
                              fig = function() flodia(all_flows))
})
