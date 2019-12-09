#' getLocalAPData provides local reg data from AngioPCIVar
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select left_join rename
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table AngioPCIVar
#' @export
#'

getLocalAPData <- function(registryName, singleRow = FALSE, ...) {
  
  # declare 'dot'
  . <- ""
  
  dbType <- "mysql"
#   query <-"
# SELECT
#   SO.HovedDato,
#   AP.Hastegrad AS ForlopsType2,
#   AP.*
# FROM
#   AngioPCIVar AP
# LEFT JOIN
#   SkjemaOversikt SO
# ON
#   AP.ForlopsID=SO.ForlopsID AND AP.AvdRESH=SO.AvdRESH"

  query <- "SELECT * FROM AngioPCIVar"
    
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for AngioPCI pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for AngioPCI pivot"
  }
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  AP <- rapbase::LoadRegData(registryName, query, dbType)
  AP <- rename(AP, ForlopsType2 = Hastegrad)

  SO <- rapbase::LoadRegData(registryName, "SELECT * FROM SkjemaOversikt")
  
  AP <- left_join(AP, SO, by = c("ForlopsID", "AvdRESH"),
                  suffix = c("", ".SO"))
  
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
      AnkomstPCIDato = ymd( AnkomstPCIDato )
      ,ApningKarDato = ymd( ApningKarDato )
      ,AvdodDato = ymd( AvdodDato )
      ,BeslEKGDato = ymd( BeslEKGDato )
      ,BesUtlEKGDato = ymd( BesUtlEKGDato )
      ,FodselsDato = ymd( FodselsDato )
      ,HovedDato = ymd( HovedDato )
      ,InnleggelseHenvisendeSykehusDato = ymd( InnleggelseHenvisendeSykehusDato )
      ,PasientRegDato = ymd( PasientRegDato )
      ,ProsedyreDato = ymd( ProsedyreDato )
      ,SymptomDato = ymd( SymptomDato )
      ,SymptomdebutDato = ymd( SymptomdebutDato )
      ,TrombolyseDato = ymd( TrombolyseDato )
      ,UtskrevetDodsdato = ymd( UtskrevetDodsdato )
      ,Utskrivningsdato = ymd( Utskrivningsdato )
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
    filter(
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
  # (ikke fullstendig, må legge til mer etter hvert)
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
      Kjonn = factor(Kjonn, 
                     levels = c( 
                       "Mann"
                       , "Kvinne"
                       , NA
                       )
                     ,ordered = TRUE
                     ,exclude = NULL # inkluderer NA i levels
                     ),
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
  



  AP

  }