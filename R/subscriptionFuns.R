#' Provide reports according to subscription
#' 
#' When called, typically through a chain of events initiated by a chron
#' process this is the function that actually produces the report to be
#' shipet off to the recipient
#'
#' @param baseName String vector with name of Rmd template for the report.
#' Must be provided without the file extention (\emph{i.e.} ".Rmd")
#' @param reshId String vector with the organization id of the subscriber 
#' @param registryName String vector naming the regisry
#' @param author String providing name of the subscriber
#' @param hospitalName String vector with a (human readable) org name
#' @param type String vector defining report file format, currently one of
#' "pdf" or "html"
#'
#' @return Full path of file produced
#' @export

subscriptionLocalMonthlyReps <- function(baseName, reshId, registryName,
                                         author, hospitalName, type) {
  
  raplog::subLogger(author = author, registryName = registryName,
                    reshId = reshId)
  
  sourceFile <- system.file(paste0(baseName, ".Rmd"), package = "noric")
  tableFormat <- switch (type,
    pdf = "latex",
    html = "html"
  )
  
  outFile <- tempfile(pattern = baseName, fileext = paste0(".", type))
  
  rmarkdown::render(input = sourceFile,
                    output_format = switch(
                      type,
                      pdf = pdf_document(),
                      html = html_document()
                    ),
                    output_file = outFile,
                    params = list(reshId=reshId,
                                  registryName=registryName,
                                  author=author,
                                  hospitalName=hospitalName,
                                  tableFormat=tableFormat),
                    clean = TRUE,
                    intermediates_dir = tempdir())
  
  outFile
  
}