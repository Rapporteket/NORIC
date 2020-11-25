#' Make standard table for rmarkdown reports
#'
#' Functions that will return tables used in reports.
#'
#' \code{mst()} creates RMarkdown code for creating standard tables.
#'
#' \code{prettyTab()} creates a table with cells containing percentages of totals
#' and observations optinally with a marginal row count
#'
#' @param tab Data frame or matrix represetnting the table
#' @param col_names Character vector with column names. Defaults
#' \code{colnames(tab)}
#' @param type Character string defining output, either 'html' or 'latex'.
#' Default is 'latex'
#' @param cap Character string with table caption. Empty string by default
#' @param label Character string defining the label in case the table needs to
#' be referenced elsewhere in the overall document. For instance, setting this
#' to 'my_table' the corresponding inline rmarkdown reference to use is
#' \code{\\@ref(tab:my_table)}. Please note that for this to work for both
#' LaTex and HTML the bookdown document processing functions must be used,
#' \emph{i.e.} bookdown:pdf_document2() and bookdown::html_document2(),
#' respectively. Default value is \code{knitr::opts_current$get("label")}.
#' @param digs Numeric number of digits to use. = by default
#' @param align Character vector specifying column alignment in the LaTeX way,
#' \emph{e.g.} \code{c("l", "c", "r")} will align the first column to the left,
#' center the second and right-aling the last one. Default is NULL in which case
#' @param fs Numeric providing the font size. Only apply for LaTeX output.
#' Default value is 8
#' @param lsd Logical if table is to be scaled down. Only apply for LaTeX
#' output. FALSE by default
#' @param add_totals Logical if a column of sums is to be added to the table.
#' Defaults to FALSE
#'
#' @return Character string containing RMarkdown table code or an R data object
#' @name makeStandardTable
#' @aliases mst prettyTab
#' @examples
#' mst(tab = mtcars[1:10, ])
#' prettyTab(tab = as.matrix(mtcars[1:10, ]))
NULL


#' @rdname makeStandardTable
#' @export
mst <- function(tab, col_names = colnames(tab), type = "latex", cap = "",
                label = "", digs = 0, align = NULL, fs = 8, lsd = FALSE) {

  if (type == "latex") {
    if (lsd) {
      lo <- c("HOLD_position", "scale_down")
    } else {
      lo <- c("HOLD_position")
    }
    k <- knitr::kable(tab, format = type, col.names = col_names, caption = cap,
                      label = label, digits = digs,
                      align = align, booktabs = TRUE) %>%
      kableExtra::kable_styling(latex_options = lo, font_size = fs)
  }

  if (type == "html") {
    k <- knitr::kable(tab, format = type, col.names = col_names, caption = cap,
                      label = label, digits = digs,
                      align = align) %>%
      kableExtra::kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)
  }
  k
}

#' @rdname makeStandardTable
#' @export
prettyTab <- function(tab, add_totals = FALSE) {
  tabR <- round(
    100 * prop.table(
      tab,
      margin = 1 ),
    digits = 1 )
  N <- sprintf( "%3.0f", tab )
  R <- sprintf( "%4.1f", tabR )
  tabTot <- paste0( N, " (", R, "%)" )

  if(add_totals == FALSE){
    newTab <- matrix(
      data = tabTot ,
      ncol = ncol(tab) ,
      byrow = FALSE )
    newTab[ which(newTab == "  0 ( 0.0%)") ] <- "     -   "
    newTab[ which(newTab == "  0 ( NaN%)") ] <- "     -   "
    rownames(newTab) <- rownames(tab)
    colnames(newTab) <- colnames(tab)
  } else {
    totals <- sprintf("%3.0f",tab %>% margin.table(1) )
    newTab <- matrix(
      data = c(tabTot, totals) ,
      ncol = ncol(tab) + 1 ,
      byrow = FALSE )
    newTab[ which(newTab == "  0 ( 0.0%)") ] <- "     -   "
    newTab[ which(newTab == "  0 ( NaN%)") ] <- "     -   "
    rownames(newTab) <- rownames(tab)
    colnames(newTab) <- c(colnames(tab),"Totalt")
  }

  newTab
}
