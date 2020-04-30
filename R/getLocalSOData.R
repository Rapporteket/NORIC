#' getLocalSOData provides local reg data from SkjemaOversikt
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table SkjemaOversikt
#' @export
#'

getLocalSOData <- function(registryName, singleRow = FALSE, ...) {
  
  # declare 'dot'
  . <- ""
  
  dbType <- "mysql"
  query <- "SELECT * FROM SkjemaOversikt"
  
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for SkjemaOversikt pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for SkjemaOversikt pivot"
  }
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  SO <- rapbase::LoadRegData(registryName, query, dbType)
  

  
  # Gjor datoer om til dato-objekt:
  SO %<>%
    mutate(
      OpprettetDato = ymd( OpprettetDato )
      ,SistLagretDato = ymd( SistLagretDato )
      ,HovedDato = ymd( HovedDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  SO %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  SO %<>%
    filter(
      (
        (AvdRESH == 102966) & ( as.Date(HovedDato) >= "2013-01-01" ) # HUS
      ) | (
        (AvdRESH == 101619) & ( as.Date(HovedDato) >= "2013-05-01" ) # UNN
      ) | (
        (AvdRESH == 109880) & ( as.Date(HovedDato) >= "2014-01-01" ) # Ullevål
      ) | (
        (AvdRESH == 104284) & ( as.Date(HovedDato) >= "2014-01-01" ) # St.Olavs
      ) | (
        (AvdRESH == 114150) & ( as.Date(HovedDato) >= "2014-01-01" ) # SSA
      ) | (
        (AvdRESH == 105502) & ( as.Date(HovedDato) >= "2014-01-01" ) # SUS
      ) | (
        (AvdRESH == 700422) & ( as.Date(HovedDato) >= "2015-01-01" ) # Riksen
      ) | (
        (AvdRESH == 106944) & ( as.Date(HovedDato) >= "2015-01-01" ) # LHLGardermoen
      ) | (
        (AvdRESH == 108141) & ( as.Date(HovedDato) >= "2016-01-01" ) # Ahus
      ) | (
        (AvdRESH == 4210141) & ( as.Date(HovedDato) >= "2020-02-10" ) # Bodø
      )
    )
  

  
  
  # Utledete variabler:
  SO %<>%
    mutate(
      # Ferdigstilt, 1= ja, -1 & 0 = nei
      ferdigstilt = as.ordered( ifelse(SkjemaStatus == 1
                                       , yes = "Ferdigstilt"
                                       , no = "Ikke ferdigstilt")
                                ),
      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      year = as.ordered( year( HovedDato )),
      aar = year,
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered( sprintf(fmt = "%02d", month( HovedDato ) )),
      maaned = as.ordered( paste0( year, "-", maaned_nr) ),
      # Kvartal:
      kvartal = quarter( HovedDato, with_year = TRUE ),
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) ),
      kvartal = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal) ),
      # Uketall:
      uke = as.ordered( sprintf(fmt = "%02d", isoweek( HovedDato ) )),
      
      # Variabel "yyyy-ukenummer" som tar høyde for uketall som befinner seg i to kalenderår:
      aar_uke = ifelse( test = uke == "01" & maaned_nr == "12", # hvis uke 01 i desember...
                        yes = paste0( as.integer(year(HovedDato)) + 1, "-", uke ), # ..sier vi at year er det seneste året som den uken tilhørte
                        no = paste0(aar, "-", uke )
      ),
      aar_uke = ifelse( test = uke %in% c("52", "53") & maaned_nr == "01", # hvis uke 52 eller 53 i januar...
                        yes = paste0( as.integer(year(HovedDato)) - 1, "-", uke ), # ...sier vi at hele uken tilhører det tidligste året
                        no = aar_uke
      ),
      aar_uke = as.ordered( aar_uke )
    ) 
  
  SO

  }