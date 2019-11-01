#' getLocalAPData provides local reg data from AngioPCIVar
#'
#' @param registryName 
#'
#' @return Data frame representing the table AndreProsedyrerVar
#' @export
#'

getLocalAPData <- function(registryName, ...) {
  
  dbType <- "mysql"
  SSQuery <-"
SELECT *
FROM SegmentStent;
"
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = "Query data for SegmentStent pivot")
  }
  
  SS <- rapbase::LoadRegData(registryName, SSQuery, dbType)
  
  
  
  # Gjor datoer om til dato-objekt:
  SS %<>%
    dplyr::mutate(
      FodselsDato = lubridate::ymd( FodselsDato )
      ,ProsedyreDato = lubridate::ymd( ProsedyreDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  SS %<>%
    dplyr::mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  SS %<>%
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
  SS %<>%
    dplyr::mutate(
      Etterdilatasjon = factor( Etterdilatasjon,
                          levels = c(
                            "Ja"
                            , "Ja, gjennom stentmaskene"
                            , "Ja, Kissing-Balloon teknikk"
                            , "Ja, separate ballonger gjennom stentmaskene og i stent"
                            , "Nei"
                            , "Ukjent"
                            , NA)
                          ,exclude = NULL # inkluderer NA i levels
                          ,ordered = TRUE
      ),
      Indikasjon = as.factor( Indikasjon ),
      Graft = factor( Graft,
                      levels = c(
                        "Nei"
                        ,"Arteriell"
                        ,"Vene"
                      )
                      ,exclude = NULL # inkluderer NA i levels
                      ,ordered = TRUE 
      ),
      LokalSuksess = factor( LokalSuksess,
                             levels = c(
                               "Ja"
                               ,"Nei"
                               ,NA
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
      ProsedyreType = factor( ProsedyreType,
                              levels = c(
                                "Annen terapi"
                                ,"Atherectomi"
                                ,"Ballong + Stent"
                                ,"Ballongdilatasjon"
                                ,"Cutting Ballon"
                                ,"Diagnostikk"
                                ,"Direktestent"
                                ,"Medikamentell ballong"
                                ,"Medikamentell ballong + Stent"
                                ,"Rotablator"
                                ,"Wireforsøk"
                              ),
                              ordered = TRUE 
                              ),
      Segment = as.ordered( Segment ),
      Stenoseklasse = factor( Stenoseklasse,
                              levels = c(
                                "A"
                                ,"B1"
                                ,"B1 Bifurkasjon"
                                ,"B2"
                                ,"B2 Bifurkasjon"
                                ,"C"
                                ,"C Bifurkasjon"
                                ,"Annet"
                              ),
                              ordered = TRUE 
                              ),
      StenoseType = factor( StenoseType,
                              levels = c(
                                "DeNovo"
                                ,"In-stent restenose"
                                ,"Stenttrombose "
                                ,"Andre restenoser"
                              ),
                              ordered = TRUE 
                              ),
      StentType = factor( StentType,
                          levels = c(
                            "DES"
                            , "BMS"
                            , "Annet"
                            , NA)
                          ,exclude = NULL # inkluderer NA i levels
                          ,ordered = TRUE
      ),
      Sykehusnavn = as.ordered( Sykehusnavn )
      
    )
  
  
  # Utledete variabler:
  SS %<>% 
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
  
  
  
  
  
  SS
  
}