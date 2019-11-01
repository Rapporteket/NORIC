#' getLocalAPData provides local reg data from AngioPCIVar
#'
#' @param registryName 
#'
#' @return Data frame representing the table AndreProsedyrerVar
#' @export
#'

getLocalAPData <- function(registryName, ...) {
  
  dbType <- "mysql"
  APQuery <- "SELECT * FROM AngioPCIVar"
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = "Query data for AngioPCI pivot")
  }
  
  AP <- rapbase::LoadRegData(registryName, APQuery, dbType)
  
  # Packages:
  require(dplyr)
  require(magrittr)
  require(lubridate)
  
  
  # Klokkeslett med "01.01.70 " som prefix fikses:
  AP %<>%
    mutate(
      ProsedyreTid = gsub( "01.01.70 " , "" , ProsedyreTid ) ,
      SymptomTid = gsub( "01.01.70 " , "" , SymptomTid ) ,
      BesUtlEKGTid = gsub( "01.01.70 " , "" , BesUtlEKGTid ) ,
      AnkomstPCITid = gsub( "01.01.70 " , "" , AnkomstPCITid ) ,
      ApningKarTid = gsub( "01.01.70 " , "" , ApningKarTid ) ,
      InnleggelseHenvisendeSykehusTid = gsub( "01.01.70 " , "" , InnleggelseHenvisendeSykehusTid ) ,
      SymptomdebutTid = gsub( "01.01.70 " , "" , SymptomdebutTid ) ,
      BeslEKGTid = gsub( "01.01.70 " , "" , BeslEKGTid ) ,
      TrombolyseTid = gsub( "01.01.70 " , "" , TrombolyseTid )
      )
  
  
  # Gjor datoer om til dato-objekt:
  AP %<>%
    mutate(
      AnkomstPCIDato = lubridate::ymd( AnkomstPCIDato )
      ,ApningKarDato = lubridate::ymd( ApningKarDato )
      ,AvdodDato = lubridate::ymd( AvdodDato )
      ,BeslEKGDato = lubridate::ymd( BeslEKGDato )
      ,BesUtlEKGDato = lubridate::ymd( BesUtlEKGDato )
      ,FodselsDato = lubridate::ymd( FodselsDato )
      ,HovedDato = lubridate::ymd( HovedDato )
      ,InnleggelseHenvisendeSykehusDato = lubridate::ymd( InnleggelseHenvisendeSykehusDato )
      ,PasientRegDato = lubridate::ymd( PasientRegDato )
      ,ProsedyreDato = lubridate::ymd( ProsedyreDato )
      ,SymptomDato = lubridate::ymd( SymptomDato )
      ,SymptomdebutDato = lubridate::ymd( SymptomdebutDato )
      ,TrombolyseDato = lubridate::ymd( TrombolyseDato )
      ,UtskrevetDodsdato = lubridate::ymd( UtskrevetDodsdato )
      ,Utskrivningsdato = lubridate::ymd( Utskrivningsdato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  AP %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  AP %<>%
    dplyr::filter(
      (
        (Sykehusnavn=="HUS") & ( as.Date(ProsedyreDato) >= "2013-01-01") # Unødvendig å bruke as.Date(), slette senere?
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
  AP %<>%
    mutate(
      ForlopsType2 = factor( ForlopsType2,
                             levels = c(
                               "Akutt"
                               , "Subakutt"
                               , "Planlagt"
                             ),
                             ordered = TRUE ),
      Indikasjon = as.factor( Indikasjon ),
      Kjonn = factor(Kjonn, levels = c( "Mann", "Kvinne"), ordered = TRUE),
      OverflyttetFra = as.factor( OverflyttetFra ),
      ProsedyreType = factor( ProsedyreType,
                              levels = c(
                                "Angio"
                                ,"Angio + PCI"
                                ,"PCI"
                              ),
                              ordered = TRUE ),
      Sykehusnavn = as.ordered( Sykehusnavn )
      
    )
  
  
  # Utledete variabler:
  AP %<>% 
    mutate( 
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
  
  



  AP

  }