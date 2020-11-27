#' getAKData provides local or national reg data from AortaklaffVar
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all mutate_at select recode left_join
#' vars
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table AortaklaffVar
#' @export
#'

getAKData <- function(registryName, singleRow = FALSE, ...) {

  # declare 'dot'
  . <- ""

  dbType <- "mysql"
  query <- "
SELECT
  *
FROM
  AortaklaffVar
"

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg <- "Query metadata for AortaklaffVar pivot"
  } else {
    query <- paste0(query, ";")
    msg <- "Query data for AortaklaffVar pivot"
  }

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }

  aK <- rapbase::LoadRegData(registryName, query, dbType)

  fO <- rapbase::LoadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")


  # Velger relevante variabler fra fO som skal legges til tabellen:
  fO %<>%
    dplyr::select(.,
      # Nøkler:
      .data$AvdRESH,
      .data$ForlopsID,
      # Variablene som legges til:
      .data$Sykehusnavn,
      .data$PasientID,
      .data$FodselsDato,
      .data$Kommune,
      .data$KommuneNr,
      .data$Fylke,
      .data$Fylkenr,
      .data$PasientKjonn,
      .data$PasientAlder,
      .data$ForlopsType1,
      .data$ForlopsType2,
      .data$KobletForlopsID,
      .data$HovedDato
    )


  aK <- dplyr::left_join(aK, fO, by = c("ForlopsID", "AvdRESH"),
                         suffix = c("", ".fO"))



  # Gjor datoer om til dato-objekt:
  aK %<>%
    dplyr::mutate_at(
      vars(ends_with("dato", ignore.case = TRUE)), list(ymd)
    )


  # Utledete variabler:
  aK %<>%
    dplyr::mutate(.,

      dager_mellom_prosedyre_og_utskr = as.numeric(
        difftime(.data$UtskrDato, .data$ProsedyreDato, units = "days")),

      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      aar = as.ordered(year(.data$ProsedyreDato)),
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      maaned_nr = as.ordered(sprintf(fmt = "%02d",
                                     month(.data$ProsedyreDato))),
      maaned = as.ordered(paste0(.data$aar, "-", .data$maaned_nr)),
      # Kvartal:
      kvartal = quarter(.data$ProsedyreDato, with_year = TRUE),
      kvartal = as.ordered(gsub("[[:punct:]]", "-Q", .data$kvartal)),
      # Uketall:
      uke = as.ordered(sprintf(fmt = "%02d", isoweek(.data$ProsedyreDato))),

      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:

      aar_uke = ifelse(
        # hvis uke 01 er i desember...
        test = .data$uke == "01" & .data$maaned_nr == "12",
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        yes = paste0(as.integer(year(.data$ProsedyreDato)) + 1, "-",
                     .data$uke),
        no = paste0(.data$aar, "-", .data$uke)
      ),
      aar_uke = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test = .data$uke %in% c("52", "53") & .data$maaned_nr == "01",
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1.januar 2017 som er i uke 52 blir til 2016-52)
        yes = paste0(as.integer(year(.data$ProsedyreDato)) - 1, "-",
                     .data$uke),
        no = .data$aar_uke
      ),
      aar_uke = as.ordered(.data$aar_uke)
    )



  aK

}