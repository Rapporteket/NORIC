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
  segmentstent;
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
   A1.AvdRESH,
   A1.ForlopsID ,
   A1.PasientID ,
   A1.ProsedyreType,
   A1.ProsedyreDato,
   A2.AvdRESH,
   A2.ForlopsID ,
   A2.PasientID ,
   SUM(S.ForlopsID>0) AS Nstents
FROM
   angiopcivardel1 A1
LEFT JOIN angiopcivardel2 A2 ON
    A1.AvdRESH = A2.AvdRESH AND
    A1.PasientID = A2.PasientID AND
    A1.ForlopsID = A2.ForlopsID
LEFT JOIN segmentstent S ON 
    A1.ForlopsID = S.ForlopsID
WHERE A1.ProsedyreType  != 'Angio'
GROUP BY ForlopsID;
"

  rapbase::loadRegData(registryName, query, dbType)

}
