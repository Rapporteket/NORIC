#' Sample data generator
#' 
#' Functions to generate sample data for development purposes. These data
#' should be distributed openly, hence it must not hold any restricted
#' or sensitive information.
#' 
#' Resulting data can be saved by:
#' save([data_frame_name], file="[filename].rda")
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
  AngioPCI <- LoadRegData(registryName, query, dbType)
  
  # remove/change data
  # no need for it here?
  
  return(AngioPCI)

}