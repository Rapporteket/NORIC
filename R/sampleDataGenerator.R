#' Sample data generator for SegmentStent
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
#' @return SegmentStent a data frame holding the data set


makeSegmentStentSampleData <- function(baseName, reshID) {

  registryName <- NORICmakeRegistryName(baseName, reshID)
  query <- "select * from SegmentStent"
  dbType<-"mysql"
  SegmentStent <- rapbase::loadRegData(registryName, query, dbType)

  # remove/change data
  nRows <- length(SegmentStent[,1])
  SegmentStent$PasientKjonn=rep("ukjent", nRows)
  SegmentStent$FodselsDato=rep("1950-01-01", nRows)
  SegmentStent$Sykehusnavn=rep("Testsykehus", nRows)
  SegmentStent$AvdRESH=rep("123456", nRows)

  return(SegmentStent)

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

  registryName <- NORICmakeRegistryName(baseName, reshID)
  query <- "
SELECT
   A.ForlopsID ,
   A.ProsedyreType ,
   A.ProsedyreDato ,
   SUM(S.ForlopsID>0) AS Nstents
FROM
   AngioPCIVar A
   LEFT JOIN SegmentStent S on A.ForlopsID=S.ForlopsID
WHERE A.ProsedyreType  != 'Angio'
GROUP BY ForlopsID;
  "
  dbType<-"mysql"
  AngioPCI <- rapbase::loadRegData(registryName, query, dbType)

  # remove/change data
  # no need for it here?

  return(AngioPCI)

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

  registryName <- NORICmakeRegistryName(baseName, reshID)
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
  dbType<-"mysql"
  AngioPCIVar <- rapbase::loadRegData(registryName, query, dbType)

  # remove/change data
  # no need for it here?

  return(AngioPCIVar)

}