#' Sample data generator
#' 
#' Functions to generate sample data for development purposes. These data
#' should be distributed openly, hence it must not hold any restricted
#' or sensitive information.
#' 
#' 
#' 


makeSegmentStentSampleData <- function(baseName, reshID) {
  
  registryName <- NORICmakeRegistryName(baseName, reshID)
  query <- "select * from SegmentStent"
  dbType<-"mysql"
  SegmentStent <- LoadRegData(registryName, query, dbType)
  
  # remove/change data
  nRows <- length(SegmentStent[,1])
  SegmentStent$PasientKjonn=rep("ukjent", nRows)
  SegmentStent$FodselsDato=rep("1950-01-01", nRows)
  SegmentStent$Sykehusnavn=rep("Testsykehus", nRows)
  SegmentStent$AvdRESH=rep("123456", nRows)
  
  return(SegmentStent)
  
}

makeAngioPCISampleData <- function(baseName, reshID) {
  
  
  return(AngioPCI)
}