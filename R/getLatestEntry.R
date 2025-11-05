#' Get the latest registration date from the registry database
#'
#' Provides the newest date based on HovedDato from skjemaoversikt
#'
#' @param registryName Character string providing registry name key
#' @param reshID Numerical for local registry
#' @return A Date object

#' @name getLatestEntry
#' @aliases getLatestEntryHospital
#' NULL
#' 
#' @rdname getLatestEntry
#' @export
getLatestEntry <- function(registryName) {

  if(registryName == "noric_nasjonal"){
    query <- "
      SELECT
        max(HovedDato) AS date
      FROM
        skjemaoversikt;"
  } else{query <- "
      SELECT
        max(mce.INTERDAT) AS date
      FROM 
        mce; "
    }
 rapbase::loadRegData(registryName, query = query)$date
}

#' @rdname getLatestEntry
#' @export
getLatestEntryHospital <- function(registryName, reshID = 99999) {
  
  if(registryName == "noric_nasjonal"){
    query <- paste0("
    SELECT
      max(HovedDato) AS date
    FROM
      skjemaoversikt 
    WHERE AvdRESH=", 
     reshID, ";"  )
  } else {
    query <- paste0("
    SELECT
      max(mce.INTERDAT) AS date
    FROM
      mce 
    WHERE mce.CENTREID = ", 
           reshID, ";"  )
  }
  rapbase::loadRegData(registryName, query = query)$date
}