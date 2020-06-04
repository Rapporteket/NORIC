#' Query data for dumps
#'
#' @param registryName String registry name
#' @param tableName String with name of table to query from
#' @param fromDate String with start period, endpoint included
#' @param toDate String with end period, endpont included
#' @param ... Additional parmeters to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr select left_join
#'
#' @return A data frame with registry data
#' @export
#'

getDataDump <- function(registryName, tableName, fromDate, toDate, ...) {
  
  # Datadumper som skal filtreres på bakgrunn av ProsedyreDato:
  # AnP, AD, AK, AP, & SS
  if( tableName %in% c( "AndreProsedyrerVar"
                        , "AnnenDiagnostikkVar"
                        , "AortaklaffVar"
                        , "AngioPCIVar"
                        , "SegmentStent") 
  ){
    query <- paste0("
SELECT
  *
FROM 
  ", tableName, " 
WHERE 
  ProsedyreDato >= '", fromDate, "' AND ProsedyreDato <= '", toDate, "';"
    )
  }
  
  
  # Datadumper som skal filtreres på bakgrunn av HovedDato:
  # SO & FO
  if (tableName %in% c( "SkjemaOversikt"
                        , "ForlopsOversikt")
  ){
    query <- paste0("
SELECT
  * 
FROM
  ", tableName, " 
WHERE
  HovedDato >= '", fromDate, "' AND HovedDato <= '", toDate, "';")
  }
  
  
  # Datadumper som skal filtreres på bakgrunn av UndersokDato:
  # CT
  if( tableName %in% c( "CTAngioVar" ) 
  ){
    query <- paste0("
SELECT
  *
FROM 
  ", tableName, "
WHERE 
  UndersokDato >= '", fromDate, "' AND UndersokDato <= '", toDate, "';"
    )
  }
  
  

  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = paste("NORIC data dump:\n", query))
  }
  
  
  # Henter tabellen som skal lastes ned av bruker:
  tab <- rapbase::LoadRegData(registryName, query)
  
  
  # Henter FO, som har felt som skal legges til tabellen (med unntak av når tabellene som
  # skal lastes ned er FO, SO, eller PasientStudier)
    if( tableName %in% c( "AndreProsedyrerVar"
                          , "AnnenDiagnostikkVar"
                          , "AortaklaffVar"
                          , "AngioPCIVar"
                          , "CTAngioVar"
                          , "SegmentStent" )
  ){
    
      
    FO <- rapbase::LoadRegData(registryName, query = "SELECT * FROM ForlopsOversikt")
    
    
    # De forskjellige tabellene har ulike felt fra FO som de har behov for, og ulike
    # nøkler. Derfor får hver tabell sin egen if-setning:
    
    
    # AnP ----
    if( tableName %in% c( "AndreProsedyrerVar" )){
      
      FO %<>% 
        select(
          # Nøkler:
          AvdRESH
          ,ForlopsID
          # Variablene som legges til:
          ,Sykehusnavn
          ,PasientID
          ,FodselsDato
          ,Kommune
          ,KommuneNr
          ,Fylke
          ,Fylkenr
          ,PasientKjonn
          ,PasientAlder
          ,ForlopsType1
          ,ForlopsType2
          ,KobletForlopsID
          ,HovedDato
        )
      
      tab <- left_join(tab, FO, by = c("ForlopsID", "AvdRESH")
                       , suffix = c("", ".FO") 
                       )
    }
    
    # AD ----
    if( tableName %in% c( "AnnenDiagnostikkVar" )){
      
      FO %<>% 
        select(
          # Nøkler:
          AvdRESH
          ,ForlopsID
          # Variablene som legges til:
          ,PasientID
          ,Kommune
          ,KommuneNr
          ,Fylke
          ,Fylkenr
          ,ForlopsType1
          ,ForlopsType2
          ,KobletForlopsID
          ,HovedDato
        )
      
      tab <- left_join(tab, FO, by = c("ForlopsID", "AvdRESH")
                       , suffix = c("", ".FO") 
                       )
    }
    
    # AK ----
    if( tableName %in% c( "AortaklaffVar" )){
      
      FO %<>% 
        select(
          # Nøkler:
          AvdRESH
          ,ForlopsID
          # Variablene som legges til:
          ,Sykehusnavn
          ,PasientID
          ,FodselsDato
          ,Kommune
          ,KommuneNr
          ,Fylke
          ,Fylkenr
          ,PasientKjonn
          ,PasientAlder
          ,ForlopsType1
          ,ForlopsType2
          ,KobletForlopsID
          ,HovedDato
        )
      
      tab <- left_join(tab, FO, by = c("ForlopsID", "AvdRESH")
                       , suffix = c("", ".FO") 
      )
    }
    
    
    # AP ----
    if( tableName %in% c( "AngioPCIVar" ) ){
      
      FO %<>% 
        select(
          # Nøkler:
          AvdRESH
          ,Sykehusnavn
          ,PasientID
          ,ForlopsID
          # FodselsDato # Finnes per dags dato i AP fra før
          # Variablene som legges til:
          ,Kommune
          ,KommuneNr
          ,Fylke
          ,Fylkenr
          # ,PasientKjonn # Finnes per dags dato i AP (heter ikke PasientKjonn, men Kjonn)
          ,PasientAlder
          ,ForlopsType1
          ,ForlopsType2
          ,KobletForlopsID
          ,HovedDato
        )
      
      # Legger til variabler fra FO til AP:
      tab <- left_join(tab, FO, by = c("AvdRESH"
                                       ,"Sykehusnavn"
                                       ,"PasientID"
                                       ,"ForlopsID") 
      ) 
    }
    
    
    # CT ----
    if( tableName %in% c( "CTAngioVar" ) ){
      
      FO %<>% 
        select(
          # Nøkler:
          AvdRESH
          ,ForlopsID
          ,PasientID
          # Variablene som legges til:
          ,Sykehusnavn
          # ,FodselsDato # Finnes per d.d. i CT
          ,Kommune
          ,KommuneNr
          ,Fylke
          ,Fylkenr
          ,PasientKjonn
          ,PasientAlder
          ,ForlopsType1
          ,ForlopsType2
          ,KobletForlopsID
          ,HovedDato
        )
      
      tab <- left_join(tab, FO, by = c("ForlopsID"
                                       ,"PasientID"
                                       , "AvdRESH"),
                      suffix = c("", ".FO") 
                      )
    }
    
    
    
    # SS ----
    if( tableName %in% c( "SegmentStent" ) ){
      
      FO %<>% 
        select(
          # Nøkler:
          AvdRESH
          ,ForlopsID
          ,Sykehusnavn
          # Variablene som legges til:
          ,PasientID
          # ,FodselsDato # Finnes per d.d. i SS
          ,Kommune
          ,KommuneNr
          ,Fylke
          ,Fylkenr
          # ,PasientKjonn # Finnes per d.d. i SS
          ,PasientAlder
          ,ForlopsType1
          ,ForlopsType2
          ,KobletForlopsID
          ,HovedDato
        )
      
      tab <- left_join(tab, FO, by = c("ForlopsID"
                                       , "AvdRESH"
                                       , "Sykehusnavn"),
                      suffix = c("", ".FO") 
                      )
    }
  }
  
  
  # Returnerer tabell (med eller uten felt fra FO)
  return( tab )
  
}
