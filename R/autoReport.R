#' Make sheduled automated reports
#'
#' When called, typically through a chain of events initiated by a chron
#' process, these are functions that actually produces the reports to be
#' shipped off to the recipients. The actual call to this function is made
#' through do.call and has the effect of providing the parameters as class
#' \emph{list}. Hence, values must be extracted by list operations
#'
#' @param baseName Single element list with value of Rmd template for the
#' report.
#' Value must be provided without the file extention (\emph{i.e.} ".Rmd")
#' @param reshId Single element list with the organization id of the subscriber
#' as its value
#' @param reshID Single element list with the organization id of the subscriber
#' as its value
#' @param userRole String defining the user role
#' @param registryName Single element list which value is the regisry name
#' @param author Single element list holding the name of the subscriber as its
#' value
#' @param hospitalName Single element list with a (human readable) org name as
#' its value
#' @param type Single element list which value defining report file format,
#' currently one of "pdf" or "html"
#'
#' @return Full path of file produced
#' @name autoReport
#' @aliases subscriptionLocalMonthlyReps dispatchMonthyKi
NULL

#' @rdname autoReport
#' @export
dispatchMonthlyKi <- function(baseName, hospitalName, reshID, author, userRole,
                              type, registryName) {

  sourceFile <- system.file(paste0(baseName, ".Rmd"), package = "noric")
  tableFormat <- switch(
    type,
    pdf = "latex",
    html = "html"
  )

  outFile <- tempfile(
    pattern = paste0(baseName, "_", as.character(Sys.Date()), "_"),
    fileext = paste0(".", type)
  )

  rmarkdown::render(input = sourceFile,
                    output_format = switch(
                      type,
                      pdf = "pdf_document",
                      html = "html_document"
                    ),
                    output_file = outFile,
                    params = list(
                      hospitalName = hospitalName,
                      reshID = reshID,
                      author = author,
                      userRole = userRole,
                      tableFormat = tableFormat,
                      registryName = registryName
                    ),
                    clean = TRUE,
                    intermediates_dir = tempdir())

  outFile
}


#' @rdname autoReport
#' @export
subscriptionLocalMonthlyReps <- function(baseName, reshId, registryName,
                                         author, hospitalName, type) {

  sourceFile <- system.file(paste0(baseName, ".Rmd"), package = "noric")
  tableFormat <- switch(type,
                        pdf = "latex",
                        html = "html"
  )

  outFile <- tempfile(pattern = baseName, fileext = paste0(".", type))

  rmarkdown::render(input = sourceFile,
                    output_format = switch(
                      type,
                      pdf = "pdf_document",
                      html = "html_document"
                    ),
                    output_file = outFile,
                    params = list(
                      reshId = reshId,
                      registryName = registryName,
                      author = author,
                      hospitalName = hospitalName,
                      tableFormat = tableFormat
                    ),
                    clean = TRUE,
                    intermediates_dir = tempdir())

  outFile

}





#' @rdname autoReport
#' @export
bulletinStaging <- function() {



}
