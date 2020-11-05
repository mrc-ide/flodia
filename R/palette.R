
#' @importFrom ghibli ghibli_palette
get_ghibli <- function() {
  palettes <-
    c("MarnieLight1", "MarnieMedium1", "MarnieDark1", "MarnieLight2",
      "MarnieMedium2", "MarnieDark2", "PonyoLight", "PonyoMedium",
      "PonyoDark", "LaputaLight", "LaputaMedium", "LaputaDark", "MononokeLight",
      "MononokeMedium", "MononokeDark", "SpiritedLight", "SpiritedMedium",
      "SpiritedDark", "YesterdayLight", "YesterdayMedium", "YesterdayDark",
      "KikiLight", "KikiMedium", "KikiDark", "TotoroLight", "TotoroMedium",
      "TotoroDark")
  out <- lapply(palettes, ghibli::ghibli_palette, n = 7)
  names(out) <- palettes
}


#'@title light palette
#'@param col either an integer vector between 1 and X or a string of the
#' following possible colour names: rd, or, oryl, yl, ylgn, gn, gnbu, bu, bupu,
#'  pu, pupi, pi, gr

light_palette <- function(col) {
  gp <- get_ghibli()
  palette <- c(rd = gp$MononokeLight[5], or = gp$MononokeLight[6],
               yl = gp$YesterdayLight[6], gn = gp$MarnieLight2[6],
               bu = gp$LaputaMedium[6], bupu = gp$TotoroLight[5],
               pu = gp$LaputaLight[5], pi = gp$TotoroLight[7], gr = "grey90")
  palette[col]
}

#'@title mid palette
#'@param col either an integer vector between 1 and X or a string of the
#' following possible colour names: rd, or, oryl, yl, ylgn, gn, gnbu, bu, bupu,
#'  pu, pupi, pi, gr

mid_palette <- function(col) {
  gp <- get_ghibli()
  gp[1]
  palette <- c(rd = "#E75B64FF", or = "#E48C2AFF", oryl = "#CD4F38FF",
    yl = "#DCCA2CFF", gn = "#58A449FF", bu = "#67B9E9FF",
    bupu = "#06425AFF", pupi = "#AE93BEFF", pu = "#5C5992FF",
    gr = "grey0.7")
  palette[col]
}

#'@title dark palette
#'@param col either an integer vector between 1 and X or a string of the
#' following possible colour names: rd, or, oryl, yl, ylgn, gn, gnbu, bu, bupu,
#'  pu, pupi, pi, gr
dark_palette <- function(col) {

}
