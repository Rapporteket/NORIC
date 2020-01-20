#' getFOData provides reg data from ForlopsOversikt
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
#' @return Data frame representing the table ForlopsOversikt
#' @export
#'

getFOData <- function(registryName, singleRow = FALSE, ...) {
  
  # declare 'dot'
  . <- ""
  
  dbType <- "mysql"
  query <-"
SELECT *
FROM ForlopsOversikt
  "
  
  # NB! Usikker på om dette er riktig:{
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for ForlopsOversikt pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for ForlopsOversikt pivot"
  }
  # }
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  FO <- rapbase::LoadRegData(registryName, query, dbType)
  
  
  # Gjor datoer om til dato-objekt:
  FO %<>%
    mutate(
      AvdodDato = ymd( AvdodDato )
      ,FodselsDato = ymd( FodselsDato )
      ,HovedDato = ymd( HovedDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  FO %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  # ForlopsOversikt inneholder ikke ProsedyreDato -> bruker derfor HovedDato til å filtrere
  
  FO %<>%
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
      ))
  
  
  # Gjøre kategoriske variabler om til factor:
  FO %<>%
    mutate(
      AvdRESH = as.ordered( AvdRESH )
      ,Sykehusnavn = as.ordered( Sykehusnavn )
      
      ,Kommune = addNA( Kommune )
      ,KommuneNr = addNA( KommuneNr )
      ,PasientKjonn = factor(PasientKjonn, 
                             levels = c( 
                               "Mann"
                               , "Kvinne"
                               , NA
                             )
                             ,ordered = TRUE
                             ,exclude = NULL # inkluderer NA i levels
      )
      , erMann = addNA( erMann )
      , Norsktalende = addNA( Norsktalende)
      , Avdod = addNA( Avdod)
      , BasisRegStatus = addNA( BasisRegStatus )
      
      ,ForlopsType1 = as.ordered( ForlopsType1 )
      ,ForlopsType1Num = as.ordered( ForlopsType1Num )
      ,ForlopsType2Num = as.ordered( ForlopsType2Num )
      ,ErOppflg = as.ordered( ErOppflg )
    )
  
  
  # Utledete variabler:
  FO %<>% 
    mutate( 
      # Div. tidsvariabler:
      #
      # Kalenderår for HovedDato:
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
  
  
  
  FO
  
}