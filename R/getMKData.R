#' getMKData provides local or national reg data from MitralklaffVar
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
#' @return Data frame representing the table MitralklaffVar
#' @export
#'

getMKData <- function(registryName, singleRow = FALSE, ...) {
  
  # declare 'dot'
  . <- ""
  
  dbType <- "mysql"
  query <-"
SELECT
  *
FROM
  MitralklaffVar
"
  
  if (singleRow) {
    query <- paste0(query, "\nLIMIT\n  1;")
    msg = "Query metadata for MitralklaffVar pivot"
  } else {
    query <- paste0(query, ";")
    msg = "Query data for MitralklaffVar pivot"
  }
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]], msg = msg)
  }
  
  MK <- rapbase::LoadRegData(registryName, query, dbType)
  
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
      ,KommuneNr
      ,Kommune
      ,Fylke
      ,Fylkenr
      ,PasientKjonn
      ,PasientAlder
      ,BasisRegStatus
      ,ForlopsType1
      ,ForlopsType2
      ,KobletForlopsID
      ,HovedDato
      ,FodselsDato
      ,Avdod
      ,AvdodDato
      # ,ErOppflg # Kommenteres ut da de kanskje skal legges til senere 
      # ,OppflgStatus
      # ,OppflgSekNr
      # ,OppflgRegStatus
    )
  
  
  MK <- left_join(MK, FO, by = c("ForlopsID", "AvdRESH"),
                  suffix = c("", ".FO"))
  
  
  # Klokkeslett med "01.01.70 " som prefix fikses:
  MK %<>%
    mutate(
      Avslutningstid = gsub( "01.01.70 " , "" , Avslutningstid ) ,
      Punksjonstid = gsub( "01.01.70 " , "" , Punksjonstid )
    )
  
  
  # Gjor datoer om til dato-objekt:
  MK %<>%
    mutate(
      Dodsdato = ymd( Dodsdato )
      ,ProsedyreDato = ymd( ProsedyreDato )
      ,FodselsDato = ymd( FodselsDato ) # FO
      ,HovedDato = ymd( HovedDato ) # FO
      ,UtskrDato = ymd( UtskrDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  MK %<>%
    mutate(
      Sykehusnavn = ifelse( Sykehusnavn == "Haukeland" , "HUS" , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn %in% c("St.Olav", "St. Olav") , "St.Olavs"  , Sykehusnavn ) ,
      Sykehusnavn = ifelse( Sykehusnavn == "Akershus universitetssykehus HF" , "Ahus" , Sykehusnavn )
    )
  
  
  # Per dags dato tar vi ikke bort forløp som har registrert ProsedyreDato fra før
  # TAVI-registreringene i NORIC startet offisielt (1/1/2017).
  
  # (Svein skrev 7/6-2019 at vi i noen tilfeller behøver eldre prosedyrer, så
  # vi filtrerer ikke etter dato i det hele tatt nå til å begynne med.
  # Kan ev. på sikt legge til en parameter hvor man kan spesifisere om
  # man vil kun ha prosedyrer fra det "offisielle" tidsrommet.)
  
  
  
  
  # Gjøre kategoriske variabler om til factor:
  # (ikke fullstendig, må legge til mer etter hvert)
  MK %<>%
    mutate(
      
      ForlopsType2 = factor( ForlopsType2,
                              levels = c(
                                "Akutt"
                                , "Subakutt"
                                , "Planlagt"
                              ),
                              ordered = TRUE )
      
      ,Frailty = factor( Frailty,
                        levels = c(
                          "Robust"
                          ,"Intermediær"
                          ,"Skrøpelig"
                          ,"Ukjent"
                          ,NA
                        )
                        ,exclude = NULL # inkluderer NA i levels
                        ,ordered = TRUE
      )
      
      ,Hastegrad = factor( Hastegrad,
                          levels = c(
                            "Elektiv"
                            , "Haster"
                            , "Akutt"
                            , "Under pågående HLR"
                            ,NA
                          )
                          ,exclude = NULL # inkluderer NA i levels
                          ,ordered = TRUE )

      ,PostVenstreVentrikkelFunksjon = factor( PostVenstreVentrikkelFunksjon,
                                             levels = c(
                                               "Normal"
                                               ,"Lett nedsatt: EF 40 - 49% "
                                               ,"Moderat nedsatt: EF 30 - 39%"
                                               ,"Betydelig nedsatt: EF 21 - 29%"
                                               ,"Alvorlig nedsatt: EF <= 20%"
                                               ,"Ukjent"
                                               ,NA
                                             )
                                             ,exclude = NULL # inkluderer NA i levels
                                             ,ordered = TRUE
      )

      ,PreVenstreVentrikkelFunksjon = factor( PreVenstreVentrikkelFunksjon,
                                             levels = c(
                                               "Normal"
                                               ,"Lett nedsatt: EF 40 - 49% "
                                               ,"Moderat nedsatt: EF 30 - 39%"
                                               ,"Betydelig nedsatt: EF 21 - 29%"
                                               ,"Alvorlig nedsatt: EF <= 20%"
                                               ,"Ukjent"
                                               ,NA
                                             )
                                             ,exclude = NULL # inkluderer NA i levels
                                             ,ordered = TRUE
      )
   
      ,ProsedyreEkko = factor( ProsedyreEkko,
                               levels = c(
                                 "Nei" 
                                 ,"TEE" 
                                 ,"ICE" 
                                 ,"TTE"
                                 ,"IVUS"
                                 ,"Annet"
                                 ,NA
                               )
                               ,exclude = NULL # inkluderer NA i levels
                               ,ordered = TRUE
      )

      ,UtskrevetTil = factor(UtskrevetTil, 
                       levels = c( 
                         "Hjem"           
                         ,"Rehabilitering" 
                         ,"Annet sykehus"
                         ,"Sykehjem"
                         , NA
                       )
                       ,exclude = NULL # inkluderer NA i levels
                       ,ordered = TRUE
      )

    )
  
  
  # Utledete variabler:
  MK %<>% 
    mutate( 
      
      dager_mellom_prosedyre_og_utskr = as.numeric( difftime(  UtskrDato, ProsedyreDato, units = "days" ) )
      
      # Div. tidsvariabler:
      #
      # Kalenderår for ProsedyreDato:
      ,year = as.ordered( year( ProsedyreDato ))
      ,aar = year
      # Måned:
      # (månedsnr er tosifret; 01, 02, ....)
      ,maaned_nr = as.ordered( sprintf(fmt = "%02d", month( ProsedyreDato ) ))
      ,maaned = as.ordered( paste0( year, "-", maaned_nr) )
      # Kvartal:
      ,kvartal = quarter( ProsedyreDato, with_year = TRUE )
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) ),
      ,kvartal = as.ordered( gsub( "[[:punct:]]", "-Q", kvartal) )
      # Uketall:
      ,uke = as.ordered( sprintf(fmt = "%02d", isoweek( ProsedyreDato ) ))
      # Variabel "yyyy-ukenummer" som tar høyde for uketall som befinner seg i to kalenderår:
      ,aar_uke = ifelse( test = uke == "01" & maaned_nr == "12", # hvis uke 01 i desember...
                        yes = paste0( as.integer(year(ProsedyreDato)) + 1, "-", uke ), # ..sier vi at year er det seneste året som den uken tilhørte
                        no = paste0(aar, "-", uke )
      )
      ,aar_uke = ifelse( test = uke %in% c("52", "53") & maaned_nr == "01", # hvis uke 52 eller 53 i januar...
                        yes = paste0( as.integer(year(ProsedyreDato)) - 1, "-", uke ), # ...sier vi at hele uken tilhører det tidligste året
                        no = aar_uke
      )
      ,aar_uke = as.ordered( aar_uke )
    )
  
  
  MK
  
}