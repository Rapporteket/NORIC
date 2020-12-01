#' getLocalCTData provides local reg data from CTAngioVar
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @return Data frame representing the table CTAngioVar
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select left_join
#' @importFrom tidyselect starts_with
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @export
#'

getLocalCTData <- function(registryName, singleRow = FALSE, ...) {

  # declare 'dot'
  . <- ""

  dbType <- "mysql"
  query <-"
SELECT *
FROM CTAngioVar
  "

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for CTAngio pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for CTAngio pivot"
  }

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }

  CT <- rapbase::loadRegData(registryName, query, dbType)

  FO <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")

  # Velger relevante variabler fra FO som skal legges til tabellen:
  FO %<>%
    select(
      # Nøkler:
      AvdRESH
      ,ForlopsID
      # Variablene som legges til:
      ,Sykehusnavn
      ,Kommune
      ,KommuneNr
      ,Fylke
      ,Fylkenr
      ,PasientKjonn
      ,PasientAlder
      ,ForlopsType1
      ,ForlopsType2
    )

  CT <- left_join(CT, FO, by = c( "ForlopsID"
                                  , "AvdRESH"
                                  # , "PasientID"
                                  ),
                   suffix = c("", ".FO"))


  # Gjor datoer om til dato-objekt:
  CT %<>%
    mutate_at(
      vars( ends_with("dato", ignore.case = TRUE) ), list( ymd )
    )


  # Endre Sykehusnavn til kortere versjoner:
  CT %<>%
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
  CT %<>%
    filter(
      (
        (AvdRESH == 102966) & ( as.Date(UndersokDato) >= "2013-01-01" ) # HUS
      ) | (
        (AvdRESH == 101619) & ( as.Date(UndersokDato) >= "2013-05-01" ) # UNN
      ) | (
        (AvdRESH == 109880) & ( as.Date(UndersokDato) >= "2014-01-01" ) # Ullevål
      ) | (
        (AvdRESH == 104284) & ( as.Date(UndersokDato) >= "2014-01-01" ) # St.Olavs
      ) | (
        (AvdRESH == 114150) & ( as.Date(UndersokDato) >= "2014-01-01" ) # SSA
      ) | (
        (AvdRESH == 105502) & ( as.Date(UndersokDato) >= "2014-01-01" ) # SUS
      ) | (
        (AvdRESH == 700422) & ( as.Date(UndersokDato) >= "2015-01-01" ) # Riksen
      ) | (
        (AvdRESH == 106944) & ( as.Date(UndersokDato) >= "2015-01-01" ) # LHLGardermoen
      ) | (
        (AvdRESH == 108141) & ( as.Date(UndersokDato) >= "2016-01-01" ) # Ahus
      ) | (
        (AvdRESH == 4210141) & ( as.Date(UndersokDato) >= "2020-02-10" ) # Bodø
      )
    )


  # Utledete variabler:
  CT %<>%
    mutate(
      # Div. tidsvariabler:
      #
      # Kalenderår for UndersokDato:
      aar = as.ordered( year( UndersokDato ))
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      ,maaned_nr = as.ordered( sprintf(fmt = "%02d", month( UndersokDato ) ))
      ,maaned = as.ordered( paste0( aar, "-", maaned_nr) )
      # Kvartal:
      ,kvartal = quarter( UndersokDato, with_year = TRUE )
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) )
      ,kvartal = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal) )
      # Uketall:
      ,uke = as.ordered( sprintf(fmt = "%02d", isoweek( UndersokDato ) ))

      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:

      ,aar_uke = ifelse(
        # hvis uke 01 er i desember...
        test = uke == "01" & maaned_nr == "12"
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        , yes = paste0( as.integer(year(UndersokDato)) + 1, "-", uke )
        , no = paste0(aar, "-", uke )
      )
      ,aar_uke = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test = uke %in% c("52", "53") & maaned_nr == "01"
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        , yes = paste0( as.integer(year(UndersokDato)) - 1, "-", uke )
        , no = aar_uke
      )
      ,aar_uke = as.ordered( aar_uke )
    )

  # Utledete variabler - opptelling av funnkoder i de 20 segmentene (ikke graft)
  CT %<>%
    mutate(
      # Opptelling av registrerte funnkoder i segmentene:
      ant_NA = (select(., starts_with("SEGMENT") ) %>% is.na() %>% rowSums() ),
      ant_0 = ( select(., starts_with("SEGMENT") ) %>%
                  mutate_all(. , list( ~( . %in% 0)) ) %>%
                  rowSums(., na.rm = TRUE) ),
      ant_10 = ( select(., starts_with("SEGMENT") ) %>%
                   mutate_all(. , list(~( . %in% 10)) ) %>%
                   rowSums(., na.rm = TRUE) ),
      ant_11 = ( select(., starts_with("SEGMENT") ) %>%
                   mutate_all(. , list(~( . %in% 11)) ) %>%
                   rowSums(., na.rm = TRUE) ),
      ant_12 = ( select(., starts_with("SEGMENT") ) %>%
                   mutate_all(. , list(~( . %in% 12)) ) %>%
                   rowSums(., na.rm = TRUE) ),
      ant_13 = ( select(., starts_with("SEGMENT") ) %>%
                   mutate_all(. , list(~( . %in% 13)) ) %>%
                   rowSums(., na.rm = TRUE) ),
      ant_14 = ( select(., starts_with("SEGMENT") ) %>%
                   mutate_all(. , list(~( . %in% 14)) ) %>%
                   rowSums(., na.rm = TRUE) ),
      ant_15 = ( select(., starts_with("SEGMENT") ) %>%
                   mutate_all(. , list(~( . %in% 15)) ) %>%
                   rowSums(., na.rm = TRUE) ),
      ant_16 = ( select(., starts_with("SEGMENT") ) %>%
                   mutate_all(. , list(~( . %in% 16)) ) %>%
                   rowSums(., na.rm = TRUE) ),
      ant_17 = ( select(., starts_with("SEGMENT") ) %>%
                   mutate_all(. , list(~( . %in% 17)) ) %>%
                   rowSums(., na.rm = TRUE) ),
      ant_18 = ( select(., starts_with("SEGMENT") ) %>%
                   mutate_all(. , list(~( . %in% 18)) ) %>%
                   rowSums(., na.rm = TRUE) ),
      # Per den nyeste nummereringen, så er obstruktiv stenose (>=50%) kodet med
      # tallene 13,14 og 15:
      ant_obstruktiv = ( select(., starts_with("SEGMENT") ) %>%
                           mutate_all(. , list(~( . %in% c(13,14,15))) ) %>%
                           rowSums(., na.rm = TRUE) )
    )



  CT

}