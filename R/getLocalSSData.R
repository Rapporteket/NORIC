#' getLocalSSData provides local reg data from SegmentStent
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr arrange count filter group_by left_join mutate mutate_at
#' select ungroup
#' @importFrom lubridate isoweek month quarter year ymd
#'
#' @return Data frame representing the table SegmentStent
#' @export
#'

getLocalSSData <- function(registryName, singleRow = FALSE, ...) {

  # declare 'dot'
  . <- ""

  dbType <- "mysql"
  query <-"
SELECT
  *
FROM
  SegmentStent
"

  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for SegmentStent pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for SegmentStent pivot"
  }

  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }

  SS <- rapbase::loadRegData(registryName, query, dbType)

  FO <- rapbase::loadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")

  # Velger relevante variabler fra FO som skal legges til tabellen:
  FO %<>%
    select(
      # Nøkler:
      AvdRESH
      ,ForlopsID
      ,Sykehusnavn
      # Variablene som legges til:
      ,PasientID
      # ,FodselsDato # Finnes per d.d. i SS
      ,Kommune
      ,KommuneNr
      ,Fylke
      ,Fylkenr
      # ,PasientKjonn # Finnes per d.d. i SS
      ,PasientAlder
      ,ForlopsType1
      ,ForlopsType2
      ,KobletForlopsID
    )

  SS <- left_join(SS, FO, by = c("ForlopsID", "AvdRESH", "Sykehusnavn"),
                  suffix = c("", ".FO"))

  # Gjor datoer om til dato-objekt:
  SS %<>%
    mutate_at(
      vars( ends_with("dato", ignore.case = TRUE) ), list( ymd )
    )


  # Endre Sykehusnavn til kortere versjoner:
  SS %<>%
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
  SS %<>%
    filter(
      (
        (AvdRESH == 102966) & ( as.Date(ProsedyreDato) >= "2013-01-01" ) # HUS
      ) | (
        (AvdRESH == 101619) & ( as.Date(ProsedyreDato) >= "2013-05-01" ) # UNN
      ) | (
        (AvdRESH == 109880) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # Ullevål
      ) | (
        (AvdRESH == 104284) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # St.Olavs
      ) | (
        (AvdRESH == 114150) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # SSA
      ) | (
        (AvdRESH == 105502) & ( as.Date(ProsedyreDato) >= "2014-01-01" ) # SUS
      ) | (
        (AvdRESH == 700422) & ( as.Date(ProsedyreDato) >= "2015-01-01" ) # Riksen
      ) | (
        (AvdRESH == 106944) & ( as.Date(ProsedyreDato) >= "2015-01-01" ) # LHLGardermoen
      ) | (
        (AvdRESH == 108141) & ( as.Date(ProsedyreDato) >= "2016-01-01" ) # Ahus
      ) | (
        (AvdRESH == 4210141) & ( as.Date(ProsedyreDato) >= "2020-02-10" ) # Bodø
      )
    )


  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  SS %<>%
    mutate(
      Graft = factor( Graft,
                      levels = c(
                        "Nei"
                        ,"Arteriell"
                        ,"Vene"
                      )
                      ,exclude = NULL # inkluderer NA i levels
                      ,ordered = TRUE
      )

      ,Stenoseklasse = factor( Stenoseklasse,
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
                              )

      ,StenoseType = factor( StenoseType,
                              levels = c(
                                "DeNovo"
                                ,"In-stent restenose"
                                ,"Stenttrombose "
                                ,"Andre restenoser"
                              ),
                              ordered = TRUE
                              )
    )


  # Utledete variabler:
  SS %<>%
    mutate(
      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      aar = as.ordered( year( ProsedyreDato ))
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      ,maaned_nr = as.ordered( sprintf(fmt = "%02d", month( ProsedyreDato ) ))
      ,maaned = as.ordered( paste0( aar, "-", maaned_nr) )
      # Kvartal:
      ,kvartal = quarter( ProsedyreDato, with_year = TRUE )
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) )
      ,kvartal = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal) )
      # Uketall:
      ,uke = as.ordered( sprintf(fmt = "%02d", isoweek( ProsedyreDato ) ))

      # Variabel med "yyyy-ukenummer" som tar høyde for uketall spredt over to
      # kalenderår:

      ,aar_uke = ifelse(
        # hvis uke 01 er i desember...
        test = uke == "01" & maaned_nr == "12"
        # .. så sier vi at uken tilhører det seneste av de to årene som uke 01
        # er spredt over (uke 01 i desember 2019 blir til 2020-01)
        , yes = paste0( as.integer(year(ProsedyreDato)) + 1, "-", uke )
        , no = paste0(aar, "-", uke )
      )
      ,aar_uke = ifelse(
        # hvis uke 52 eller 53 er i januar...
        test = uke %in% c("52", "53") & maaned_nr == "01"
        # ...sier vi at hele uken tilhører det tidligste av de to årene som uke
        # 52/53 er spredt over (1. januar 2017 som er i uke 52 blir til 2016-52)
        , yes = paste0( as.integer(year(ProsedyreDato)) - 1, "-", uke )
        , no = aar_uke
      )
      ,aar_uke = as.ordered( aar_uke )
    )


  # Utledet variabel:
  # ant_stent_ila_forlop = antall stenter satt inn ila ett forløp

  SS %<>%
    group_by( Sykehusnavn, ForlopsID ) %>%
    mutate( antall_stent_ila_forlop = sum( !is.na(StentType) ) ) %>%
    dplyr::ungroup() %>%
    arrange( Sykehusnavn, ForlopsID )


  SS

}