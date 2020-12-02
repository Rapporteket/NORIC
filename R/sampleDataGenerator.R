#' Sample data generator for segmentStent
#'
#' Functions to generate sample data for development purposes. These data
#' should be distributed openly, hence it must not hold any restricted
#' or sensitive information.
#'
#' Resulting data can be saved by:
#' save([data_frame_name], file="[filename].rda")
#'
#' @param baseName String giving the prefix base of the name
#' @param reshID String providing the current reshID. At Rapporteket, reshID
#'  should already be present in the current R session
#' @return segmentStent a data frame holding the data set


makeSegmentStentSampleData <- function(baseName, reshID) {

  registryName <- noric::makeRegistryName(baseName, reshID)
  query <- "select * from segmentStent"
  dbType <- "mysql"
  segmentStent <- rapbase::loadRegData(registryName, query, dbType)

  # remove/change data
  nRows <- length(segmentStent[, 1])
  segmentStent$PasientKjonn <- rep("ukjent", nRows)
  segmentStent$FodselsDato <- rep("1950-01-01", nRows)
  segmentStent$Sykehusnavn <- rep("Testsykehus", nRows)
  segmentStent$AvdRESH <- rep("123456", nRows)

  segmentStent

}

#' Sample data generator for AngioPCI
#'
#' Functions to generate sample data for development purposes. These data
#' should be distributed openly, hence it must not hold any restricted
#' or sensitive information.
#'
#' Resulting data can be saved by:
#' save([data_frame_name], file="[filename].rda")
#'
#' @param baseName String giving the prefix base of the name
#' @param reshID String providing the current reshID. At Rapporteket, reshID
#'  should already be present in the current R session
#' @return AngioPCI a data frame holding the data set

makeAngioPCISampleData <- function(baseName, reshID) {

  registryName <- noric::makeRegistryName(baseName, reshID)
  query <- "
SELECT
   A.ForlopsID ,
   A.ProsedyreType ,
   A.ProsedyreDato ,
   SUM(S.ForlopsID>0) AS Nstents
FROM
   AngioPCIVar A
   LEFT JOIN segmentStent S on A.ForlopsID=S.ForlopsID
WHERE A.ProsedyreType  != 'Angio'
GROUP BY ForlopsID;
  "
  dbType <- "mysql"

  rapbase::loadRegData(registryName, query, dbType)

}


#' Sample data generator for AngioPCIVar
#'
#' Functions to generate sample data for development purposes. These data
#' should be distributed openly, hence it must not hold any restricted
#' or sensitive information.
#'
#' Resulting data can be saved by:
#' save([data_frame_name], file="[filename].rda")
#'
#' @param baseName String giving the prefix base of the name
#' @param reshID String providing the current reshID. At Rapporteket, reshID
#'  should already be present in the current R session
#' @return AngioPCIVar a data frame holding the data set

makeTimeToTreatmentSampleData <- function(baseName, reshID) {

  registryName <- noric::makeRegistryName(baseName, reshID)
  query <- "
SELECT
  Sykehusnavn ,
  OverflyttetFraSykehus ,
  OverflyttetFra ,
  Indikasjon ,
  AnkomstPCIDato ,
  InnleggelseHenvisendeSykehusDato ,
  ProsedyreDato ,
  PrimaerForlopsID
FROM
   AngioPCIVar;
  "
  dbType <- "mysql"
  rapbase::loadRegData(registryName, query, dbType)

}