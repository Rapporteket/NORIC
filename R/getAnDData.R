#' getAnDData provides local reg data from AnnenDiagnostikkVar
#'
#' @param registryName String providing the registry name
#' @param singleRow Logical defining if only one row is to be returned. A
#' relevant usecase will be when only description is needed. By default set to
#' FALSE
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select left_join
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table AnnenDiagnostikkVar
#' @export
#'

getAnDData <- function(registryName, singleRow = FALSE, ...) {
  
  # declare 'dot'
  . <- ""
  
  dbType <- "mysql"
  query <-"
SELECT *
FROM AnnenDiagnostikkVar
  "
  
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for AnnenDiagnostikk pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for AnnenDiagnostikk pivot"
  }
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  AnD <- rapbase::LoadRegData(registryName, query, dbType)
  
  FO <- rapbase::LoadRegData(registryName,
                             query = "SELECT * FROM ForlopsOversikt")
  
  
  # Velger relevante variabler fra FO som skal legges til tabellen:
  FO %<>% 
    select(
      # Nøkler:
      AvdRESH
      ,Sykehusnavn
      ,ForlopsID
      # Variablene som legges til:
      # FodselsDato # Finnes per dags dato i AnD fra før
      ,PasientID
      ,Kommune
      ,KommuneNr
      ,Fylke
      ,Fylkenr
      # ,PasientKjonn # Finnes per dags dato i AnD fra før
      ,PasientAlder
      ,ForlopsType1
      ,ForlopsType2
      ,KobletForlopsID
      ,HovedDato
    )
  
  # Legger til variabler fra FO til AnD:
  AnD <- left_join(AnD, FO, by = c("AvdRESH"
                                   ,"Sykehusnavn"
                                   ,"ForlopsID"
                                   ) 
  ) 
  
  
  
  # Gjor datoer om til dato-objekt:
  AnD %<>%
    mutate(
      ProsedyreDato = ymd( ProsedyreDato ),
      FodselsDato = ymd( FodselsDato ),
      HovedDato = ymd( HovedDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  AnD %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  
  # Tar bort forløp fra før sykehusene ble offisielt med i NORIC (potensielle
  # "tøyseregistreringer")
  AnD %<>%
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
  AnD %<>%
    mutate(
      ForlopsType1 = factor( ForlopsType2,
                             levels = c(
                               "Angio"
                               , "Angio+PCI"
                               , "PCI"
                             ),
                             ordered = TRUE )
      ,ForlopsType2 = factor( ForlopsType2,
                             levels = c(
                               "Akutt"
                               , "Subakutt"
                               , "Planlagt"
                             ),
                             ordered = TRUE )
      ,Sykehusnavn = as.ordered( Sykehusnavn )
      ,PasientKjonn = factor(PasientKjonn, 
                     levels = c( 
                       "Mann"
                       , "Kvinne"
                       , NA
                     )
                     ,ordered = TRUE
                     ,exclude = NULL # inkluderer NA i levels
      )
      ,Indikasjon = as.factor( Indikasjon )
      ,segment = factor(segment, 
                     levels = c( 
                       "Proximale RCA (1)",
                       "Midtre RCA (2)",
                       "Distale RCA (3)",
                       "PDA/RPD (4)",
                       "Ve hovedstamme (5)",
                       "Proximale LAD (6)",
                       "Midtre LAD (7)",
                       "Distale LAD (8)",
                       "Første diagonal (9)",
                       "Andre diagonal (10)",
                       "Proximale LCx (11)",
                       "Første obtusa marginal (12)",
                       "Andre obtusa marginal (13)",
                       "Distale LCx (14)",
                       "LPD (15)",
                       "PLA fra venstre (16)",
                       "Intermediær (17)",
                       "PLA (18)",
                       "Høyrekammergren (19)",
                       "Septal (20)"
                     )
                     ,ordered = TRUE
      )
      ,graft = factor(graft, 
                     levels = c( 
                       "Arterie"
                       ,"Vene"
                       ,"Nei"
                     )
                     ,ordered = TRUE
      )
      ,metode = factor(metode, 
                     levels = c( 
                       "iFR"
                       ,"FFR"
                       ,"OCT"
                       ,"IVUS"
                       ,"CFR"
                       ,"IMR"
                       ,"Pd/Pa"
                       ,"NIRS"
                       ,"Pa-hyperemi"
                       ,"Pd-hyperemi"
                     )
                     ,ordered = TRUE
      )
    )
  
  
  # Utledete variabler:
  AnD %<>% 
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
  
  
  
  
  AnD
  
}