#' getLocalAnPData provides local reg data from AndreProsedyrerVar
#'
#' @param registryName 
#'
#' @return Data frame representing the table AndreProsedyrerVar
#' @export
#'

getLocalAnPData <- function(registryName) {
  
  dbType <- "mysql"
  AnPQuery <- "SELECT * FROM AndreProsedyrerVar"
  
  AnP <- rapbase::LoadRegData(registryName, AnPQuery, dbType)
  
  showN <- 12 # how many months are displayed
  
  AnP$ProsedyreDato <- as.Date( AnP$ProsedyreDato)
  
  AnP$Year <- as.numeric(
    format(
      x = AnP $ ProsedyreDato ,
      format = "%Y"))
  
  AnP$nMonth <- as.numeric(
    as.factor(
      format(
        AnP$ProsedyreDato ,
        format = "%y-%m")))
  
  AnP $ Day <- as.numeric(
    AnP $ ProsedyreDato - min( AnP $ ProsedyreDato , na.rm = TRUE ) )
  
  AnP <- subset(
    x = AnP ,
    subset = nMonth >= max( nMonth , na.rm = TRUE ) - showN )
  
  AnP $ Month <- as.factor(
    format(
      x = AnP $ ProsedyreDato ,
      format = "%y-%m"))
  
  AnP$AnnenProsType <- factor(
    x = AnP$AnnenProsType ,
    levels = c(
      "Høyre hjertekateterisering" ,
      "Temporær pacemaker" ,
      "Perikardiocentese" ,
      "Ventilfilming" ,
      "Lukking av ASD/PFO" ,
      "Lukking av venstre aurikkel" ,
      "Impella") ,
    labels = c(
      "Høyre kat." ,
      "Temp. pm" ,
      "Perikardiocentese" ,
      "Ventilfilming" ,
      "Lukking ASD/PFO" ,
      "Lukking v. aurikkel" ,
      "Impella")
  )
  
  AnP

  }