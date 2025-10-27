#' Common report processor for Noric
#'
#' Makes reports for Noric typically used for auto reports such as
#' subscriptions, dispatchments and bulletins. As such, please be warned that
#' any changes to this function might render existing auto reports nonfunctional
#' as they are based on static calls based on any previous version of this
#' function. Changes should therefore be thoroughly tested against existing auto
#' reports. Altering the names of the arguments will likely be a breaking
#' change. Adding new arguments should be safe as long as they are provided a
#' default value.
#'
#' @param report Character string identifying the report to be processed by this
#' function.
#' @param outputType Character string with output format. Must be one of
#' \code{c("html", "pdf")}. Defaults to "pdf".
#' @param title Character string giving the report title. Empty string by
#' default.
#' @param author Character string providing the report author. Default value is
#' "unknown author".
#' @param orgName Character string with the name of the organization/hospital.
#' Default is "unknown organization".
#' @param orgId Integer (?) with the id of the organization/hospital. Default is
#' 999999.
#' @param registryName Character string with registry name. Default is
#' "noric".
#' @param userFullName Character string giving the person name, normally the
#' user requesting the report. Default is "unknown person name".
#' @param userRole Character string giving a user role, normally the one of the
#' user requesting the report. Default is "unknown role".
#' @param userOperator Character string with some name of an operator, whatever
#' that is... Default is "unknown operator".
#' @param rendered_by_shiny boolean. if TRUE progression of pdf-generation is
#' returned.
#' @param tableFormat Character string giving the format of the report. Must be 
#' one of \code{c("html", "latex")}. Default is "latex".
#'
#' @return A character string with a path to where the produced file is located.
#' @export
#'
#' @examples
#' ## Make the start page for noric
#' \dontrun{reportFilePath <- reportProcessor(report = "veiledning",
#'                                   title = "Example report")
#' }

reportProcessor <- function(report,
                            outputType = "pdf",
                            title = "",
                            author = "unknown author",
                            orgName = "unknown organization",
                            orgId = 999999,
                            registryName = "noric",
                            userFullName = "unknown person name",
                            userRole = "unknown role",
                            userOperator = "unknown operator",
                            rendered_by_shiny = FALSE,
                            tableFormat = "latex") {

  stopifnot(report %in% c("veiledning",
                          "NORIC_local_monthly",
                          "NORIC_local_monthly_activity",
                          "NORIC_kvalitetsindikator", 
                          "NORIC_filvask_avdod", 
                          "NORIC_tavi_report"))

  stopifnot(registryName %in% c("noric_nasjonal",
                          "noric_ahus",
                          "noric_bergen",
                          "noric_bodoe", 
                          "noric_feiring", 
                          "noric_ous_rh", 
                          "noric_ous_ull", 
                          "noric_soerlandet",
                          "noric_stavanger", 
                          "noric_stolav", 
                          "noric_unn"))
 
  filePath <- NULL

  if (title == "") {
    warning("No title given! Reports should have a title...")
  }

  message(paste0(
    "Creating report: ", report, " with output type: ", outputType,
    " and table format: ", tableFormat,
    " for organization: ", orgName, " (ID: ", orgId, ")",
    " and registry: ", registryName, "."
  ))

  # For testing:
  if (report == "veiledning") {
    filePath <- rapbase::renderRmd(
      sourceFile =  system.file("veiledning.Rmd",
                                package = "noric"),
      outputType = "html",
      params = list(
        author = "author",
        hospitalName = "orgName",
        tableFormat = "html",
        reshId = "orgId"
      )
    )
  }



  if (report == "NORIC_local_monthly") {
    filePath <- rapbase::renderRmd(
      sourceFile =  system.file("NORIC_local_monthly.Rmd",
                                package = "noric"),
      outputType = outputType,
      params = list(
        author = author,
        hospitalName = orgName,
        tableFormat = tableFormat,
        
        reshId = orgId,
        registryName = registryName,
        userFullName = userFullName,
        userRole = userRole, 
        rendered_by_shiny = rendered_by_shiny
      )
    )
  }


  if (report == "NORIC_local_monthly_activity") {
    filePath <- rapbase::renderRmd(
      system.file("NORIC_local_monthly_activity.Rmd",
                  package = "noric"),
      outputType = outputType,
      params = list(
        author = author,
        hospitalName = orgName,
        tableFormat = tableFormat,
        
        reshId = orgId,
        registryName = registryName,
        userFullName = userFullName,
        userRole = userRole, 
        rendered_by_shiny = rendered_by_shiny
      )
    )
  }




  if (report == "NORIC_kvalitetsindikator") {
    
    # CHECK: 10 hospitals in AP and 5 hospitals in AK
    hospInAp <- noric::getPresentHospitalsAp(registryName = registryName)
    hospInAk <- noric::getPresentHospitalsAk(registryName = registryName)
    stopifnot("Hospital(s) missing in AP"= all(c(102966, 101619, 
                                                 104284, 105502, 
                                                 106944, 108141,
                                                 109880, 114150, 
                                                 4210141,700422)
                                               %in% hospInAp))
    stopifnot("Hospital(s) missing in AK"= all(c(102966, 101619, 
                                                 104284, 109880, 
                                                 700422)
                                               %in% hospInAk))
    
    filePath <- rapbase::renderRmd(
      sourceFile =  system.file("NORIC_kvalitetsindikator.Rmd",
                                package = "noric"),
      outputType = outputType,
      params = list(
        author = author,
        hospitalName = orgName,
        tableFormat = tableFormat,

        reshId = orgId,
        registryName = registryName,
        userFullName = userFullName,
        userRole = userRole,
        rendered_by_shiny = rendered_by_shiny
      )
    )
  }

  
  if (report == "NORIC_filvask_avdod") {
    filePath <- rapbase::renderRmd(
      sourceFile =  system.file("NORIC_filvask_avdod.Rmd",
                                package = "noric"),
      outputType = outputType,
      params = list(
        author = author,
        hospitalName = orgName,
        tableFormat = tableFormat,
        
        reshId = orgId,
        registryName = registryName,
        userFullName = userFullName,
        userRole = userRole, 
        rendered_by_shiny = rendered_by_shiny
      )
    )
  }
  
  if (report == "NORIC_tavi_report") {
    filePath <- rapbase::renderRmd(
      sourceFile =  system.file("NORIC_tavi_report.Rmd",
                                package = "noric"),
      outputType = outputType,
      params = list(
        author = author,
        hospitalName = orgName,
        tableFormat = tableFormat,
        
        reshId = orgId,
        registryName = registryName,
        userFullName = userFullName,
        userRole = userRole, 
        rendered_by_shiny = rendered_by_shiny
      )
    )
  }
  
  filePath
}
