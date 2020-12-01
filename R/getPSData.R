#' getPSData provides local or national reg data from PasienterStudier
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select recode left_join
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table PasienterStudier
#' @export
#'

getPSData <- function(registryName, singleRow = FALSE, ...) {

  # declare 'dot'
  . <- ""

  dbType <- "mysql"
  query <-"
SELECT
  *
FROM
  PasienterStudier
"

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for PasienterStudier pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for PasienterStudier pivot"
  }

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }

  PS <- rapbase::loadRegData(registryName, query, dbType)

  FO <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")


  # Velger relevante variabler fra FO som skal legges til tabellen:
  FO %<>%
    select(
      # Nøkler:
      AvdRESH
      ,PasientID
      # Variablene som legges til:
      ,Sykehusnavn
      ,FodselsDato
      ,Kommune
      ,KommuneNr
      ,Fylke
      ,Fylkenr
      ,PasientKjonn
      ,PasientAlder
    )


  PS <- left_join(PS, FO, by = c("PasientID", "AvdRESH"),
                  suffix = c("", ".FO"))



  # Gjor datoer om til dato-objekt:
  PS %<>%
    mutate_at(
      vars( ends_with("dato", ignore.case = TRUE) ), list( ymd )
    )


  # Endre Sykehusnavn til kortere versjoner:
  PS %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse(
        Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn
      ) ,
      Sykehusnavn = ifelse(
        Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn
      )
    )





  # Utledete variabler:
  PS %<>%
    mutate(

      # Div. tidsvariabler:

      # Basert på PasInklDato:

      # Kalenderår:
      aar_pasientinklusjon = as.ordered( year( PasInklDato ))
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      ,maaned_nr_pasientinklusjon =
        as.ordered( sprintf(fmt = "%02d", month( PasInklDato ) ))
      ,maaned_pasientinklusjon =
        as.ordered(
          paste0( aar_pasientinklusjon, "-", maaned_nr_pasientinklusjon)
        )
      # Kvartal:
      ,kvartal_pasientinklusjon = quarter( PasInklDato, with_year = TRUE )
      ,kvartal_pasientinklusjon =
        as.ordered( gsub( "[[:punct:]]", "-Q", kvartal_pasientinklusjon) )
      # Uketall:
      ,uke_pasientinklusjon =
        as.ordered( sprintf(fmt = "%02d", isoweek( PasInklDato ) ))

      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:
      ,aar_uke_pasientinklusjon = ifelse(
        # hvis uke 01 er i desember...
        test = uke_pasientinklusjon == "01" & maaned_nr_pasientinklusjon == "12"
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        ,yes =
          paste0( as.integer(year(PasInklDato)) + 1, "-", uke_pasientinklusjon )
        ,no = paste0(aar_pasientinklusjon, "-", uke_pasientinklusjon )
      )
      ,aar_uke_pasientinklusjon = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test = uke_pasientinklusjon %in% c("52", "53") &
          maaned_nr_pasientinklusjon == "01"
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        ,yes =
          paste0( as.integer(year(PasInklDato)) - 1, "-", uke_pasientinklusjon )
        ,no = aar_uke_pasientinklusjon
      )
      ,aar_uke_pasientinklusjon = as.ordered( aar_uke_pasientinklusjon )

      # Basert på StudieStartDato:

      # Kalenderår:
      ,aar_studiestart = as.ordered( year( StudieStartDato ))
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      ,maaned_nr_studiestart =
        as.ordered( sprintf(fmt = "%02d", month( StudieStartDato ) ))
      ,maaned_studiestart =
        as.ordered( paste0( aar_studiestart, "-", maaned_nr_studiestart) )
      # Kvartal:
      ,kvartal_studiestart = quarter( StudieStartDato, with_year = TRUE )
      ,kvartal_studiestart =
        as.ordered( gsub( "[[:punct:]]", "-Q", kvartal_studiestart) )
      # Uketall:
      ,uke_studiestart =
        as.ordered( sprintf(fmt = "%02d", isoweek( StudieStartDato ) ))
      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:
      ,aar_uke_studiestart = ifelse(
        # hvis uke 01 er i desember...
        test = uke_studiestart == "01" & maaned_nr_studiestart == "12"
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        ,yes =
          paste0( as.integer(year(StudieStartDato)) + 1, "-", uke_studiestart )
        ,no = paste0(aar_studiestart, "-", uke_studiestart )
      )
      ,aar_uke_studiestart = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test =
          uke_studiestart %in% c("52", "53") & maaned_nr_studiestart == "01"
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        ,yes =
          paste0( as.integer(year(StudieStartDato)) - 1, "-", uke_studiestart )
        ,no = aar_uke_studiestart
      )
      ,aar_uke_studiestart = as.ordered( aar_uke_studiestart )
    )



  PS

}