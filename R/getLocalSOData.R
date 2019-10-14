#' getLocalAPData provides local reg data from AngioPCIVar
#'
#' @param registryName 
#'
#' @return Data frame representing the table AndreProsedyrerVar
#' @export
#'

getLocalSOData <- function(registryName, ...) {
  
  dbType <- "mysql"
  SOQuery <- "SELECT * FROM SkjemaOversikt"
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = "Query data for SkjemaOversikt pivot")
  }
  
  SO <- rapbase::LoadRegData(registryName, SOQuery, dbType)
  
  showN <- 12 # how many months are displayed
  
  SO$RegDate <- as.Date(
    SO$HovedDato)
  
  SO$Year <- as.numeric(
    format(
      x = SO$RegDate ,
      format = "%Y"))
  
  SO$nMonth <- as.numeric(
    as.factor(
      format(
        x = SO$RegDate ,
        format = "%y-%m")))
  
  SO <- subset(
    x = SO ,
    subset =
      (nMonth > max( nMonth , na.rm = TRUE ) - showN) &
      (RegDate < Sys.Date()))
  
  
  # crude fix for Feiring and Rikshospitalet which have meaningless test data before 2015
  if ( SO$Sykehusnavn[1] %in% c("Feiring","Rikshospitalet")) SO <- SO [ which( SO$Year >= 2015 ) , ]
  
  SO$Month <- as.factor(
    format(
      SO$RegDate ,
      format = "%y-%m"))
  
  SO

  }