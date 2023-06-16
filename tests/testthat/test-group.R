test_that("group function works as expected", {
  si <- function(x_root = 0, y_root = 0) {
    s <- node(x_root, y_root, 0.1, "S")
    i <- node(s$x + 0.5, s$y, 0.1, "I")
    flowx(s, i, pos = 0.7)
    flowx(i, s, pos = 0.3)
    list(x0 = s$x0, x1 = i$x1, y0 = s$y0, y1 = s$y1, S = s)
  }

  vax <- function() {
    ygap <- 0.3
    u <- group(si, label = "unvaccinated", label_font = 3, label_cex = 0.9,
               label_col = mid_palette("pugy"),
               group_col = light_palette("pugy"),
               border_col = mid_palette("gy"), lwd = 2, lty = 2)
    v <- group(si, list(x_root = 0, y_root = u$y0 - ygap), label = "vaccinated",
               label_x = calc_pos(u$x0, u$x1, 0.4), label_y = u$y0 - ygap,
               oma = c(1, 2, 3, 4) / 30)
    flow(u$S, v$S)
    flow(v, u)

    list(x0 = u$x0, x1 = u$x1, y0 = v$y0, y1 = u$y1)
  }

  vdiffr::expect_doppelganger("Group diagram", fig = function() flodia(vax))

})

test_that("can connect groups with fork arrows", {
  f <- function(x, y) node(x, y)
  g <- function() {
    a <- f(x = 0, y = 0)
    b <- group(f, list(x = 1, y = 0))
    arr <- forky(to0 = a, to1 = b, length = -1)
    list(x0 = a$x0, x1 = b$x1, y0 = a$y0, y1 = arr$y1)
  }

  vdiffr::expect_doppelganger("Group diagram 2", fig = function() flodia(g))
})
