logo <- function() {
  r <- 0.02
  aw <- 0.1
  xgap <- 0.06
  ygap <- 0.07

  f <- node(0, 1, r, "F", node_col = light_palette("bupu"))
  l <- node(f$x + xgap, f$y, r, "L", node_col = light_palette("ylgn"))
  o <- node(l$x + xgap, f$y, r, "O", node_col = light_palette("gnbu"))
  d <- node(f$x, f$y - ygap, r, "D", node_col = light_palette("gn"))
  i <- node(l$x, d$y, r, "I", node_col = light_palette("bu"))
  a <- node(o$x, d$y, r, "A", node_col = light_palette("pu"))

  flow(f, l, arr_width = aw)
  flow(l, o, arr_width = aw)
  turny(o, calc_pos(o$y0, d$y1), d, arr_width = aw)
  flow(d, i, arr_width = aw)
  flow(i, a, arr_width = aw)

  list(x0 = f$x0, x1 = o$x1, y0 = d$y0, y1 = f$y1)
}

flodia_png(logo, "scripts/logo.png", width = 1200, res = 600,
           oma = 0.01)


