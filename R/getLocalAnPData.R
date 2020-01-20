#' getLocalAnPData provides local reg data from AndreProsedyrerVar
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select left_join
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table AndreProsedyrerVar
#' @export
#'

getLocalAnPData <- function(registryName, singleRow = FALSE, ...) {
  
  # declare 'dot'
  . <- ""
  
  dbType <- "mysql"
  query <- "
SELECT *
FROM AndreProsedyrerVar
  "
  
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for AndreProsedyrer pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for AndreProsedyrer pivot"
  }
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  AnP <- rapbase::LoadRegData(registryName, query, dbType)
  
  FO <- rapbase::LoadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")
  
  
  # Velger relevante variabler fra FO som skal legges til tabellen:
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
  
  AnP <- left_join(AnP, FO, by = c("ForlopsID", "AvdRESH"),
                  suffix = c("", ".FO"))
  

  # Klokkeslett med "01.01.70 " som prefix fikses:
  AnP %<>%
    mutate(
      ProsedyreTid = gsub( "01.01.70 " , "" , ProsedyreTid )
    )
  
  # Gjor datoer om til dato-objekt:
  AnP %<>%
    mutate(
      FodselsDato = ymd( FodselsDato )
      ,HovedDato = ymd( HovedDato )
      ,ProsedyreDato = ymd( ProsedyreDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  AnP %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  AnP %<>%
    filter(
      (
        (AvdRESH == 102966) & ( as.Date(ProsedyreDato) >= "2013-01-01") # HUS
      ) | (
        (AvdRESH == 101619) & ( as.Date(ProsedyreDato) >= "2013-05-01" ) # UNN
      ) | (
        (AvdRESH == 109880) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # Ullevål
      ) | (
        (AvdRESH == 104284) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # St.Olavs
      ) | (
        (AvdRESH == 114150) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # SSA
      ) | (
        (AvdRESH == 105502) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # SUS
      ) | (
        (AvdRESH == 700422) & ( as.Date(ProsedyreDato) >= "2015-01-01" ) # Riksen
      ) | (
        (AvdRESH == 106944) & ( as.Date(ProsedyreDato) >= "2015-01-01" ) # LHLGardermoen
      ) | (
        (AvdRESH == 108141) & ( as.Date(ProsedyreDato) >= "2016-01-01" ) # Ahus
      ))
  
  
  
  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legget til mer etter hvert)
  AnP %<>%
    mutate(
      AnnenProsType = factor( AnnenProsType,
                                levels = c(
                                  "Aortaballongpumpe"           
                                  ,"ECMO"                        
                                  ,"Høyre hjertekateterisering"  
                                  ,"Impella"                    
                                  ,"Lukking av ASD/PFO"          
                                  ,"Lukking av venstre aurikkel" 
                                  ,"Perikardiocentese"           
                                  ,"PTSMA"                      
                                  ,"Temporær pacemaker"          
                                  ,"Ventilfilming"
                                  # ,"Ikke registrert" 
                                ),
                                ordered = TRUE
      ),
      ForlopsType1 = as.factor( ForlopsType1 ),
      # (Hastegrad finnes ikke i AndreProsedyrerVar)
      ForlopsType2 = factor( ForlopsType2,
                             levels = c(
                               "Akutt"
                               , "Subakutt"
                               , "Planlagt"
                             ),
                             ordered = TRUE ),
      PasientKjonn = factor(PasientKjonn, levels = c( "Mann", "Kvinne"), ordered = TRUE),
      Sykehusnavn = as.ordered( Sykehusnavn )
      
    )
  
  
  AnP %<>%
    mutate( 
      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      year = as.ordered( year( ProsedyreDato )),
      aar = year,
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered( sprintf(fmt = "%02d", month( ProsedyreDato ) )),
      maaned = as.ordered( paste0( year, "-", maaned_nr) ),
      # Kvartal:
      kvartal = quarter( ProsedyreDato, with_year = TRUE ),
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) ),
      kvartal = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal) ),
      # Uketall:
      uke = as.ordered( sprintf(fmt = "%02d", isoweek( ProsedyreDato ) )),
      
      # Variabel "yyyy-ukenummer" som tar høyde for uketall som befinner seg i to kalenderår:
      aar_uke = ifelse( test = uke == "01" & maaned_nr == "12", # hvis uke 01 i desember...
                        yes = paste0( as.integer(year(ProsedyreDato)) + 1, "-", uke ), # ..sier vi at year er det seneste året som den uken tilhørte
                        no = paste0(aar, "-", uke )
      ),
      aar_uke = ifelse( test = uke %in% c("52", "53") & maaned_nr == "01", # hvis uke 52 eller 53 i januar...
                        yes = paste0( as.integer(year(ProsedyreDato)) - 1, "-", uke ), # ...sier vi at hele uken tilhører det tidligste året
                        no = aar_uke
      ),
      aar_uke = as.ordered( aar_uke )
    )
  
  
  AnP

  }