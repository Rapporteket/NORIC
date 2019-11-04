#' getLocalCTData provides local reg data from CTAngioVar
#'
#' @param registryName 
#'
#' @return Data frame representing the table CTAngioVar
#' 
#' @importFrom dplyr filter mutate mutate_all select
#' @importFrom tidyselect starts_with
#' @export
#'

getLocalCTData <- function(registryName, ...) {
  
  dbType <- "mysql"
  CTQuery <-"
SELECT
  FO.HovedDato,
  FO.Sykehusnavn,
  FO.ForlopsType1,
  FO.ForlopsType2,
  FO.PasientKjonn,
  CT.*
FROM
  CTAngioVar CT
LEFT JOIN
  ForlopsOversikt FO
ON
  CT.ForlopsID=FO.ForlopsID AND CT.AvdRESH=FO.AvdRESH;
"
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = "Query data for CTAngio pivot")
  }
  
  CT <- rapbase::LoadRegData(registryName, CTQuery, dbType)
  
  
  
  # Gjor datoer om til dato-objekt:
  CT %<>%
    mutate(
      AvdodDato = lubridate::ymd( AvdodDato )
      ,FodselsDato = lubridate::ymd( FodselsDato )
      ,HovedDato = lubridate::ymd( HovedDato )
      ,PasientRegDato = lubridate::ymd( PasientRegDato )
      ,UndersokDato = lubridate::ymd( UndersokDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  CT %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  CT %<>%
    filter(
      (
        (Sykehusnavn=="HUS") & ( as.Date(UndersokDato) >= "2013-01-01") # Unødvendig å bruke as.Date(), slette senere?
      ) | (
        (Sykehusnavn=="UNN") & ( as.Date(UndersokDato) >= "2013-05-01" )
      ) | (
        (Sykehusnavn=="Ullevål") & ( as.Date(UndersokDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="St.Olavs") & ( as.Date(UndersokDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="Sørlandet") & ( as.Date(UndersokDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="SUS") & ( as.Date(UndersokDato) >= "2014-01-01" )
      ) | (
        (Sykehusnavn=="Rikshospitalet") & ( as.Date(UndersokDato) >= "2015-01-01" )
      ) | (
        (Sykehusnavn=="Feiring") & ( as.Date(UndersokDato) >= "2015-01-01" )
      ) | (
        (Sykehusnavn=="Ahus") & ( as.Date(UndersokDato) >= "2016-01-01" )
      ))
  
  
  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legget til mer etter hvert)
  CT %<>%
    mutate(
      Avdod = factor( Avdod,
                             levels = c(
                               "Ja"
                               , "Nei"
                             ),
                             ordered = TRUE ),
      ForlopsType1 = as.factor( ForlopsType1 ),
      ForlopsType2 = factor( ForlopsType2,
                             levels = c(
                               "Akutt"
                               , "Subakutt"
                               , "Planlagt"
                             ),
                             ordered = TRUE ),
      Indikasjon = as.factor( Indikasjon ),
      # n.b. Både "Kjonn" og "PasientKjonn" i mine datadumper....
      Kjonn = factor(Kjonn, 
                     levels = c( 
                       "Mann"
                       , "Kvinne"
                       , NA
                     )
                     ,exclude = NULL # inkluderer NA i levels
                     ,ordered = TRUE
      ),
      PasientKjonn = factor(PasientKjonn, 
                            levels = c( 
                              "Mann"
                              , "Kvinne"
                              , NA
                            )
                            ,exclude = NULL # inkluderer NA i levels
                            ,ordered = TRUE
      ),
      Royking = factor(Royking, 
                       levels = c( 
                         "Aldri røykt"
                         , "Eks-røyker >1 mnd"
                         , "Røyker"
                         , "Ukjent"
                         , NA
                       )
                       ,exclude = NULL # inkluderer NA i levels
                       ,ordered = TRUE
      ),
      Sykehusnavn = as.ordered( Sykehusnavn ),
      VidereUtredning = factor(VidereUtredning, 
                       levels = c( 
                         "Billedbasert iskemitest"
                         , "Generell kardiologisk vurdering"
                         ,"Kirurgisk behandling"
                         , "Koronar angiografi"
                         , "Ny CT-angio på et senere tidspunkt"
                         , "Ingen videre utredning "
                         , "Ukjent"
                         , NA
                       )
                       ,exclude = NULL # inkluderer NA i levels
                       ,ordered = TRUE
      ),
      ViktigsteSymptomer = factor(ViktigsteSymptomer, 
                       levels = c( 
                         "Brystsmerter"
                         , "Dyspnoe"
                         ,"Ingen (risikoestimering)"
                         , "Ukjent"
                         , NA
                       )
                       ,exclude = NULL # inkluderer NA i levels
                       ,ordered = TRUE
      )
      
    )
  
  
  # Utledete variabler:
  CT %<>% 
    mutate( 
      # Div. tidsvariabler:
      #
      # Kalenderår for UndersokDato:
      year = as.ordered( lubridate::year( UndersokDato )),
      aar = year,
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered( sprintf(fmt = "%02d", lubridate::month( UndersokDato ) )),
      maaned = as.ordered( paste0( year, "-", maaned_nr) ),
      # Kvartal:
      kvartal = lubridate::quarter( UndersokDato, with_year = TRUE ),
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) ),
      kvartal = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal) ),
      # Uketall:
      uke = as.ordered( sprintf(fmt = "%02d", lubridate::isoweek( UndersokDato ) ))
      # På sikt: årstall-uke, "2019-34" feks, må tenke ut en lur løsning siden en og samme uke uke kan spenne fra ett år til det neste..
    )
  
  # Utledete variabler - opptelling av funnkoder i de 20 segmentene (ikke graft)
  CT %<>% 
    mutate( 
      # Opptelling av registrerte funnkoder i segmentene:
      ant_NA = (select(., starts_with("SEGMENT") ) %>% is.na() %>% rowSums() ), 
      ant_0 = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list( ~( . == 0)) ) %>% rowSums(., na.rm = TRUE) ),
      ant_10 = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list(~( . == 10)) ) %>% rowSums(., na.rm = TRUE) ), 
      ant_11 = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list(~( . == 11)) ) %>% rowSums(., na.rm = TRUE) ), 
      ant_12 = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list(~( . == 12)) ) %>% rowSums(., na.rm = TRUE) ), 
      ant_13 = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list(~( . == 13)) ) %>% rowSums(., na.rm = TRUE) ), 
      ant_14 = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list(~( . == 14)) ) %>% rowSums(., na.rm = TRUE) ), 
      ant_15 = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list(~( . == 15)) ) %>% rowSums(., na.rm = TRUE) ), 
      ant_16 = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list(~( . == 16)) ) %>% rowSums(., na.rm = TRUE) ), 
      ant_17 = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list(~( . == 17)) ) %>% rowSums(., na.rm = TRUE) ), 
      ant_18 = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list(~( . == 18)) ) %>% rowSums(., na.rm = TRUE) ), 
      # Per den nyeste nummereringen, så er obstruktiv stenose (>=50%) kodet med tallene 13,14 og 15:
      ant_obstruktiv = ( select(., starts_with("SEGMENT") ) %>% mutate_all(. , list(~( . %in% c(13,14,15))) ) %>% rowSums(., na.rm = TRUE) )
    )
  
  
  
  CT
  
}