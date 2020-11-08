
#' @importFrom ghibli ghibli_palette
get_ghibli <- function() {
  list(MarnieLight1 = c("#95918EFF", "#AF9699FF", "#80C7C9FF",
                        "#8EBBD2FF", "#E3D1C3FF", "#B3DDEBFF", "#F3E8CCFF"),
       MarnieMedium1 = c("#28231DFF", "#5E2D30FF", "#008E90FF",
                         "#1C77A3FF", "#C5A387FF", "#67B8D6FF", "#E9D097FF"),
       MarnieDark1 = c("#15110EFF", "#2F1619FF", "#004749FF",
                       "#0E3B52FF", "#635143FF", "#335D6BFF", "#73684CFF"),
       MarnieLight2 = c("#8E938DFF", "#94A39CFF", "#97B8AFFF",
                        "#A2D1BDFF", "#C0CDBCFF", "#ACD2A3FF", "#E6E58BFF"),
       MarnieMedium2 = c("#1D271CFF", "#274637FF", "#2C715FFF",
                         "#44A57CFF", "#819A7AFF", "#58A449FF", "#CEC917FF"),
       MarnieDark2 = c("#0E130DFF", "#14231CFF", "#17382FFF",
                       "#22513DFF", "#404D3CFF", "#2C5223FF", "#66650BFF"),
       PonyoLight = c("#A6A0A0FF", "#ADB7C0FF", "#94C5CCFF",
                      "#F4ADB3FF", "#EEBCB1FF", "#ECD89DFF", "#F4E3D3FF"),
       PonyoMedium = c("#4C413FFF", "#5A6F80FF", "#278B9AFF",
                       "#E75B64FF", "#DE7862FF", "#D8AF39FF", "#E8C4A2FF"),
       PonyoDark = c("#262020FF", "#2D3740FF", "#14454CFF",
                     "#742D33FF", "#6E3C31FF", "#6C581DFF", "#746353FF"),
       LaputaLight = c("#898D90FF", "#8D93A1FF", "#9F99B5FF",
                       "#AFACC9FF", "#D7CADEFF", "#DAEDF3FF", "#F7EABDFF"),
       LaputaMedium = c("#14191FFF", "#1D2645FF", "#403369FF",
                        "#5C5992FF", "#AE93BEFF", "#B4DAE5FF", "#F0D77BFF"),
       LaputaDark = c("#090D10FF", "#0D1321FF", "#1F1935FF",
                      "#2F2C49FF", "#574A5EFF", "#5A6D73FF", "#776A3DFF"),
       MononokeLight = c("#838A90FF", "#BA968AFF", "#9FA7BEFF",
                         "#B3B8B1FF", "#E7A79BFF", "#F2C695FF", "#F5EDC9FF"),
       MononokeMedium = c("#06141FFF", "#742C14FF", "#3D4F7DFF",
                          "#657060FF", "#CD4F38FF", "#E48C2AFF", "#EAD890FF"),
       MononokeDark = c("#030A10FF", "#3A160AFF", "#1F273EFF",
                        "#333831FF", "#67271BFF", "#724615FF", "#756D49FF"),
       SpiritedLight = c("#8F9297FF", "#9A9C97FF", "#C19A9BFF",
                         "#C7C0C8FF", "#B4DCF5FF", "#E1D7CBFF", "#DBEBF8FF"),
       SpiritedMedium = c("#1F262EFF", "#353831FF", "#833437FF",
                          "#8F8093FF", "#67B9E9FF", "#C3AF97FF", "#B7D9F2FF"),
       SpiritedDark = c("#0F1217FF", "#1A1C17FF", "#411A1BFF",
                        "#474048FF", "#345C75FF", "#61574BFF", "#5B6B78FF"),
       YesterdayLight = c("#768185FF", "#7E8C97FF", "#88988DFF",
                          "#9DAFC3FF", "#B1D5BBFF", "#ECE28BFF", "#C3DAEAFF"),
       YesterdayMedium = c("#061A21FF", "#132E41FF", "#26432FFF",
                           "#4D6D93FF", "#6FB382FF", "#DCCA2CFF", "#92BBD9FF"),
       YesterdayDark = c("#030E12FF", "#0B1924FF", "#15251AFF",
                         "#2A3C50FF", "#3E6248FF", "#796F18FF", "#506777FF"),
       KikiLight = c("#8E8C8FFF", "#9A9AA2FF", "#D98594FF",
                     "#86C2DAFF", "#D0C1AAFF", "#C0DDE1FF", "#E9DBD0FF"),
       KikiMedium = c("#1C1A1FFF", "#333544FF", "#B50A2AFF",
                      "#0E84B4FF", "#9E8356FF", "#7EBAC2FF", "#D1B79EFF"),
       KikiDark = c("#0E0C0FFF", "#1A1A22FF", "#590514FF",
                    "#06425AFF", "#50412AFF", "#405D61FF", "#695B50FF"),
       TotoroLight = c("#85898AFF", "#959492FF", "#AC9D96FF",
                       "#A8A6A9FF", "#A1B1C8FF", "#D6C0A9FF", "#DCD3C4FF"),
       TotoroMedium = c("#0A1215FF", "#2D2A25FF", "#583B2BFF",
                        "#534C53FF", "#446590FF", "#AD8152FF", "#BBA78CFF"),
       TotoroDark = c("#05090AFF", "#151412FF", "#2C1D16FF",
                      "#282629FF", "#213148FF", "#564029FF", "#5C5344FF")
  )
}

#'@title light palette
#'@param col either an integer vector between 1 and 15 or a string of the
#' following possible colour names: rd, or, oryl, yl, ylgn, gn, gnbu, bu, bupu,
#'  pu, pupi, pi, gr
#'@export
light_palette <- function(col) {
  gp <- get_ghibli()
  palette <- c(rd = gp$KikiLight[3], rdor = gp$MononokeLight[5],
               or = gp$MononokeLight[6], oryl = gp$MononokeMedium[7],
               yl = gp$YesterdayLight[6], ylgn = gp$MarnieLight2[7],
               gn = gp$MarnieLight2[6], gnbu = gp$PonyoLight[3],
               bu = gp$LaputaMedium[6], bupu = gp$TotoroLight[5],
               pu = gp$LaputaLight[5], pugy = gp$SpiritedLight[4],
               gy = "grey85", gybr = gp$TotoroLight[7],
               br = gp$KikiLight[7])
  palette[col]
}

#'@title mid palette
#'@param col either an integer vector between 1 and 15 or a string of the
#' following possible colour names: rd, or, oryl, yl, ylgn, gn, gnbu, bu, bupu,
#'  pu, pupi, pi, gr
#'  @export
mid_palette <- function(col) {
  gp <- get_ghibli()
  palette <- c(rd = gp$SpiritedMedium[3], rdor = gp$MononokeMedium[5],
               or = gp$MononokeMedium[6], oryl = gp$PonyoMedium[6],
               yl = gp$LaputaMedium[7], ylgn = gp$MarnieMedium2[7],
               gn = gp$MarnieMedium2[6], gnbu = gp$PonyoMedium[3],
               bu = gp$YesterdayMedium[7], bupu = gp$LaputaMedium[4],
               pu = gp$LaputaMedium[5], pugy = gp$SpiritedMedium[4],
               gy = "grey70", gybn = gp$TotoroMedium[7],
               br = gp$TotoroMedium[6])
  palette[col]
}
