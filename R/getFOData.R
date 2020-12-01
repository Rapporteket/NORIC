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

  FO <- rapbase::loadRegData(registryName, query, dbType)


  # Gjor datoer om til dato-objekt:
  FO %<>%
    mutate_at(
      vars( ends_with("dato", ignore.case = TRUE) ), list( ymd )
    )


  # Endre Sykehusnavn til kortere versjoner:
  FO %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse(
        Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn
      ) ,
      Sykehusnavn = ifelse(
        Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn
      )
    )

  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  # ForlopsOversikt inneholder ikke ProsedyreDato. Derfor brukes HovedDato til å
  # filtrere.

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
      ) | (
        (AvdRESH == 4210141) & ( as.Date(HovedDato) >= "2020-02-10" ) # Bodø
      )
    )


  # Utledete variabler:
  FO %<>%
    mutate(
      # Div. tidsvariabler:
      #
      # Kalenderår for HovedDato:
      aar = as.ordered( year( HovedDato ))
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      ,maaned_nr = as.ordered( sprintf(fmt = "%02d", month( HovedDato ) ))
      ,maaned = as.ordered( paste0( aar, "-", maaned_nr) )
      # Kvartal:
      ,kvartal = quarter( HovedDato, with_year = TRUE )
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) )
      ,kvartal = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal) )
      # Uketall:
      ,uke = as.ordered( sprintf(fmt = "%02d", isoweek( HovedDato ) ))

      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:

      ,aar_uke = ifelse(
        # hvis uke 01 er i desember...
        test = uke == "01" & maaned_nr == "12"
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        , yes = paste0( as.integer(year(HovedDato)) + 1, "-", uke )
        , no = paste0(aar, "-", uke )
      )
      ,aar_uke = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test = uke %in% c("52", "53") & maaned_nr == "01"
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        , yes = paste0( as.integer(year(HovedDato)) - 1, "-", uke )
        , no = aar_uke
      )
      ,aar_uke = as.ordered( aar_uke )
    )



  FO

}