#' Provide dataframe for stents
#'
#' Provides a dataframe for stents from NORIC staging db. To be used in
#' NORIC_local_monthly_stent.Rnw and maybe others
#'
#' @param registryName String providing the current registryName
#' @return RegData data frame
#' @export

loadRegDataStent <- function(registryName) {

  dbType <- "mysql"

  query <- "
SELECT
  *
FROM
  SegmentStent;
"

  rapbase::loadRegData(registryName, query, dbType)

}


#' Provide dataframe for AngioPCI
#'
#' Provides a dataframe for AngioPCI from NORIC staging db. To be used in
#' NORIC_local_monthly_stent.Rnw and maybe others
#'
#' @param registryName String providing the current registryName
#' @return RegData data frame
#' @export

loadRegDataAngioPCI <- function(registryName) {

  dbType <- "mysql"

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

  rapbase::loadRegData(registryName, query, dbType)

}