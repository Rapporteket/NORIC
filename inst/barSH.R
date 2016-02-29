
barSH <- function(TAB) {
    colPrimary <- c("#000059", "#084594", "#2171b5", "#4292c6", "#6baed6", "#c6dbef")

    TABr <- round(
        100 * prop.table(
                  TAB ,
                  margin = 1 ),
        digits = 1)

    par(
        mar = c(3,5,2,10) ,
        xpd = NA ,
        las = 1)

    xBarplot <- barplot(
        TABr ,
        beside = TRUE ,
        ylab = "" ,
        col = colPrimary )

    text(
        x = xBarplot[1,] ,
        y = TABr[1,] ,
        pos = 1 ,
        col = "#FFFFFFBB" ,
        labels = TAB[1,])

    text(
        x = xBarplot[2,] ,
        y = TABr[2,] ,
        pos = 1 ,
        col = "#FFFFFFAA" ,
        labels = TAB[2,])

    legend(
        x = par("usr")[2] ,
        y = par("usr")[4]*.7 ,
        bty = "n" ,
        legend = levels( A[ , names(dimnames(TAB))[1] ] ) ,
        fill = colPrimary )
    
}
