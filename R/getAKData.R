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
  query <-"
SELECT
  *
FROM
  AortaklaffVar
"
  
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for AortaklaffVar pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for AortaklaffVar pivot"
  }
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  AK <- rapbase::LoadRegData(registryName, query, dbType)
  
  FO <- rapbase::LoadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")
  
  
  # Velger relevante variabler fra FO som skal legges til tabellen:
  FO %<>% 
    select(
      # Nøkler:
      AvdRESH
      ,ForlopsID
      # Variablene som legges til:
      ,Sykehusnavn
      ,PasientID
      ,FodselsDato
      ,Kommune
      ,KommuneNr
      ,Fylke
      ,Fylkenr
      ,PasientKjonn
      ,PasientAlder
      ,ForlopsType1
      ,ForlopsType2
      ,KobletForlopsID
      ,HovedDato
    )
  
  
  AK <- left_join(AK, FO, by = c("ForlopsID", "AvdRESH"),
                  suffix = c("", ".FO"))
  
  

  # Gjor datoer om til dato-objekt:
  AK %<>%
    mutate_at(
      vars( ends_with("dato", ignore.case = TRUE) ), list( ymd )
    ) 


  # Utledete variabler:
  AK %<>%
    mutate(
      
      dager_mellom_prosedyre_og_utskr = as.numeric( 
        difftime(  UtskrDato, ProsedyreDato, units = "days" ) 
        )
      
      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      ,aar = as.ordered( year( ProsedyreDato ))
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

  

  AK
  
}