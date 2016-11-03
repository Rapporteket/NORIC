#' Generates a barplot of column percentages calculated from a count crosstabulation
#'
#' @param TabN NxM-crosstable with absolute counts
#' @return NULL
#' @export

# auxillary function called by the plot function to add the 
AnnotateN <- function(row) text(
                               x = xBarplot ,
                               y = if( row == 1 ) TabR[ 1 ,] else colSums(TabR[ 1:row ,]) ,
                               pos = 1 ,
                               col = "#000000AA" ,
                               labels = TabN[ row ,])


BarplotTable <- function( TabN , sort.level = 0 ){
    
    pal <- RColorBrewer::brewer.pal(
                             n = nrow( TabN ) ,
                             name = "Blues" )
                             
    par(
        mar = c(2,3,1,12) ,
        xpd = NA ,
        las = 1)
    
    TabR <- 100 * prop.table(
                      x = TabN ,
                      margin = 2 )

    if (sort.level %in% (1:nrow(TabR))) {
        col.index <- order( TabR[ sort.level , ] )
        TabN <- TabN[ , col.index ]
        TabR <- TabR[ , col.index ]
    }
    
    xBarplot <- barplot(
        TabR ,
        beside = FALSE ,
        horiz = FALSE ,
        axisnames = TRUE ,
        xlab = "" ,
        col = pal )
    
    lapply(
        1 : nrow( TabR ) ,
        AnnotateN
    )

    legend(
        x = par("usr")[2] ,
        y = par("usr")[4]*.7 ,
        bty = "n" ,
        title = "Timer" ,
        legend = rownames(TabR)[3:1],
        fill = pal[3:1] )
}
