#' Provide reports according to subscription
#'
#' When called, typically through a chain of events initiated by a chron
#' process, this is the function that actually produces the report to be
#' shiped off to the recipient. The actual call to this function is made
#' through do.call and has the effect of providing the parameters as class
#' \emph{list}. Hence, values must be extracted by list operations
#'
#' @param baseName Single element list with value of Rmd template for the
#' report.
#' Value must be provided without the file extention (\emph{i.e.} ".Rmd")
#' @param reshId Single element list with the organization id of the subscriber
#' as its value
#' @param registryName Single element list which value is the regisry name
#' @param author Single element list holding the name of the subscriber as its
#' value
#' @param hospitalName Single element list with a (human readable) org name as
#' its value
#' @param type Single element list which value defining report file format,
#' currently one of "pdf" or "html"
#'
#' @return Full path of file produced
#' @export

subscriptionLocalMonthlyReps <- function(baseName, reshId, registryName,
                                         author, hospitalName, type) {

  raplog::subLogger(author = author[[1]], registryName = registryName[[1]],
                    reshId = reshId[[1]],
                    msg = "Subscription report: stent/prosedyre")

  sourceFile <- system.file(paste0(baseName[[1]], ".Rmd"), package = "noric")
  tableFormat <- switch(type[[1]],
    pdf = "latex",
    html = "html"
  )

  outFile <- tempfile(pattern = baseName[[1]], fileext = paste0(".", type[[1]]))

  rmarkdown::render(input = sourceFile,
                    output_format = switch(
                      type[[1]],
                      pdf = "pdf_document",
                      html = "html_document"
                    ),
                    output_file = outFile,
                    params = c(reshId, registryName, author, hospitalName,
                               list(tableFormat = tableFormat)),
                    clean = TRUE,
                    intermediates_dir = tempdir())

  outFile

}