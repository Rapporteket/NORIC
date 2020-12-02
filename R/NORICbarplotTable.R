#' Generates a barplot of column percentages calculated from a
#' count crosstabulation
#'
#' @param tabN NxM-crosstable with absolute counts
#' @return NULL
#' @export

# auxillary function called by the plot function to add the
annotateN <- function(row) {
    text(x = xBarplot,
         y = if (row == 1) tabR[1, ] else colSums(tabR[1:row, ]),
         pos = 1,
         col = "#000000AA",
         labels = tabN[row, ])
}

barplotTable <- function(tabN, sort.level = 0) {

    pal <- RColorBrewer::brewer.pal(
        n = nrow(tabN),
        name = "Blues")

    par(mar = c(2, 3, 1, 12),
        xpd = NA,
        las = 1)

    tabR <- 100 * prop.table(
        x = tabN,
        margin = 2)

    if (sort.level %in% (seq_len(nrow(tabR)))) {
        col.index <- order(tabR[sort.level, ])
        tabN <- tabN[, col.index]
        tabR <- tabR[, col.index]
    }

    xBarplot <- barplot(
        tabR,
        beside = FALSE,
        horiz = FALSE,
        axisnames = TRUE,
        xlab = "",
        col = pal)

    lapply(
        seq_len(nrow(tabR)),
        annotateN
    )

    legend(
        x = par("usr")[2],
        y = par("usr")[4] * .7,
        bty = "n",
        title = "Timer",
        legend = rownames(tabR)[3:1],
        fill = pal[3:1])
}
