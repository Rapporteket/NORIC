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
  
  
  # Packages:
  require(dplyr)
  require(magrittr)
  require(lubridate)
  

  
  # Gjor datoer om til dato-objekt:
  SO %<>%
    mutate(
      OpprettetDato = lubridate::ymd( OpprettetDato )
      ,SistLagretDato = lubridate::ymd( SistLagretDato )
      ,HovedDato = lubridate::ymd( HovedDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  SO %<>%
    dplyr::mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  SO %<>%
    dplyr::filter(
      (
        (Sykehusnavn=="HUS") & ( as.Date(HovedDato) >= "2013-01-01")
      ) | (
        (Sykehusnavn=="UNN") & ( as.Date(HovedDato) >= "2013-05-01" )
      ) | (
        (Sykehusnavn=="Ullevål") & ( as.Date(HovedDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="St.Olavs") & ( as.Date(HovedDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="St.Olav") & ( as.Date(HovedDato) >= "2014-01-01" ) # N.B. Grunnet krøll med navnet til St. Olavs
      ) | (
        (Sykehusnavn=="Sørlandet") & ( as.Date(HovedDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="SUS") & ( as.Date(HovedDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="Rikshospitalet") & ( as.Date(HovedDato) >= "2015-01-01" )
      ) | (
        (Sykehusnavn=="Feiring") & ( as.Date(HovedDato) >= "2015-01-01" )
      ) | (
        (Sykehusnavn=="Ahus") & ( as.Date(HovedDato) >= "2016-01-01" )
      ))
  
  
  # Gjøre kategoriske variabler om til factor:
  SO %<>%
    dplyr::mutate(
      Skjemanavn = as.ordered( Skjemanavn ),
      Sykehusnavn = as.ordered( Sykehusnavn )
    )
  
  
  # Utledete variabler:
  SO %<>%
    dplyr::mutate(
      # Ferdigstilt, 1= ja, -1 & 0 = nei
      ferdigstilt = as.ordered( ifelse(SkjemaStatus == 1
                                       , yes = "Ferdigstilt"
                                       , no = "Ikke ferdigstilt")
                                ),
      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      year = as.ordered( lubridate::year( HovedDato )),
      aar = year,
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered( sprintf(fmt = "%02d", lubridate::month( HovedDato ) )),
      maaned = as.ordered( paste0( year, "-", maaned_nr) ),
      # Kvartal:
      kvartal = lubridate::quarter( HovedDato, with_year = TRUE ),
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) ),
      kvartal = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal) ),
      # Uketall:
      uke = as.ordered( sprintf(fmt = "%02d", lubridate::isoweek( HovedDato ) ))
      # På sikt: årstall-uke, "2019-34" feks, må tenke ut en lur løsning siden en og samme uke uke kan spenne fra ett år til det neste..
    ) 
  
  

  SO

  }