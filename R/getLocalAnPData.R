#' getLocalAnPData provides local reg data from AndreProsedyrerVar
#'
#' @param registryName String providing the registry name
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#'
#' @return Data frame representing the table AndreProsedyrerVar
#' @export
#'

getLocalAnPData <- function(registryName, ...) {
  
  # declare 'dot'
  . <- ""
  
  dbType <- "mysql"
  AnPQuery <- "
SELECT
  FO.HovedDato,
  FO.Sykehusnavn,
  FO.ForlopsType1,
  FO.ForlopsType2,
  FO.PasientKjonn,
  AnP.*
FROM
  AndreProsedyrerVar AnP
LEFT JOIN
  ForlopsOversikt FO
ON
  AnP.ForlopsID=FO.ForlopsID AND AnP.AvdRESH=FO.AvdRESH;"
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = "Query data for AndreProsedyrer pivot")
  }
  
  AnP <- rapbase::LoadRegData(registryName, AnPQuery, dbType)
  

  # Klokkeslett med "01.01.70 " som prefix fikses:
  AnP %<>%
    dplyr::mutate(
      ProsedyreTid = gsub( "01.01.70 " , "" , ProsedyreTid )
    )
  
  # Gjor datoer om til dato-objekt:
  AnP %<>%
    dplyr::mutate(
      ProsedyreDato = lubridate::ymd( ProsedyreDato ),
      HovedDato = lubridate::ymd( HovedDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  AnP %<>%
    dplyr::mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  AnP %<>%
    dplyr::filter(
      (
        (Sykehusnavn=="HUS") & ( as.Date(ProsedyreDato) >= "2013-01-01")
      ) | (
        (Sykehusnavn=="UNN") & ( as.Date(ProsedyreDato) >= "2013-05-01" )
      ) | (
        (Sykehusnavn=="Ullevål") & ( as.Date(ProsedyreDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="St.Olavs") & ( as.Date(ProsedyreDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="Sørlandet") & ( as.Date(ProsedyreDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="SUS") & ( as.Date(ProsedyreDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="Rikshospitalet") & ( as.Date(ProsedyreDato) >= "2015-01-01" )
      ) | (
        (Sykehusnavn=="Feiring") & ( as.Date(ProsedyreDato) >= "2015-01-01" )
      ) | (
        (Sykehusnavn=="Ahus") & ( as.Date(ProsedyreDato) >= "2016-01-01" )
      ))
  
  
  
  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legget til mer etter hvert)
  AnP %<>%
    dplyr::mutate(
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
    dplyr::mutate( 
      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      year = as.ordered( lubridate::year( ProsedyreDato )),
      aar = year,
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered( sprintf(fmt = "%02d", lubridate::month( ProsedyreDato ) )),
      maaned = as.ordered( paste0( year, "-", maaned_nr) ),
      # Kvartal:
      kvartal = lubridate::quarter( ProsedyreDato, with_year = TRUE ),
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) ),
      kvartal = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal) ),
      # Uketall:
      uke = as.ordered( sprintf(fmt = "%02d", lubridate::isoweek( ProsedyreDato ) ))
      # På sikt: årstall-uke, "2019-34" feks, må tenke ut en lur løsning siden en og samme uke uke kan spenne fra ett år til det neste..
    )
  
  
  AnP

  }