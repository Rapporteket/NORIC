#' getAKData provides local or national reg data from AortaklaffVar
#'
#' @param registryName String providing the registry name
#' @param ... Optional arguments to be passed to the function
#'
#' @importFrom magrittr %>% %<>%
#' @importFrom dplyr filter mutate mutate_all select recode
#' @importFrom lubridate ymd year month quarter isoweek
#'
#' @return Data frame representing the table AortaklaffVar
#' @export
#'

getAKData <- function(registryName, ...) {
  
  # declare 'dot'
  . <- ""
  
  dbType <- "mysql"
  AKQuery <-"
SELECT *
FROM AortaklaffVar;
"
  
  if ("session" %in% names(list(...))) {
    raplog::repLogger(session = list(...)[["session"]],
                      msg = "Query data for AngioPCI pivot")
  }
  
  AK <- rapbase::LoadRegData(registryName, AKQuery, dbType)
  
  
  
  # Klokkeslett med "01.01.70 " som prefix fikses:
  AK %<>%
    mutate(
      Avslutningstid = gsub( "01.01.70 " , "" , Avslutningstid ) ,
      Punksjonstid = gsub( "01.01.70 " , "" , Punksjonstid )
    )
  
  
  # Gjor datoer om til dato-objekt:
  AK %<>%
    mutate(
      BeslutningsDato = ymd( BeslutningsDato )
      ,Dodsdato = ymd( Dodsdato )
      ,ProsedyreDato = ymd( ProsedyreDato )
      ,UtskrDato = ymd( UtskrDato )
    )
  
  
  # Endre Sykehusnavn til kortere versjoner:
  AK %<>%
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
  # (ikke fullstendig, må legget til mer etter hvert)
  AK %<>%
    mutate(
      Aortaforkalk = factor( Aortaforkalk,
                              levels = c(
                                "Nei"
                                ,"Mild"
                                ,"Moderat"
                                ,"Alvorlig"
                                ,"Ukjent"
                                ,NA
                              )
                              ,exclude = NULL # inkluderer NA i levels
                              ,ordered = TRUE
      ),
      
      CanadianClass = factor( CanadianClass,
                              levels = c(
                                "Ikke angina/brystsmerter"
                                ,"I"
                                ,"II"
                                ,"III"
                                ,"IV"
                                ,"Ukjent"
                                ,NA
                              )
                              ,exclude = NULL # inkluderer NA i levels
                              ,ordered = TRUE
      ),
      
      Frailty = factor( Frailty,
                              levels = c(
                                "Robust"
                                ,"Intermediær"
                                ,"Skrøpelig"
                                ,"Ukjent"
                                ,NA
                              )
                              ,exclude = NULL # inkluderer NA i levels
                              ,ordered = TRUE
      ),
      
      Hastegrad = factor( Hastegrad,
                          levels = c(
                            "Elektiv"
                            , "Haster"
                            , "Akutt"
                            , "Under pågående HLR"
                            ,NA
                          )
                          ,exclude = NULL # inkluderer NA i levels
                          ,ordered = TRUE ),
      
      Indikasjon = factor( Indikasjon,
                           levels = c(
                             "Stenose"
                             ,"Stenose og insuffisiens"
                             ,"Insuffisiens"
                             ,NA
                           )
                           ,exclude = NULL # inkluderer NA i levels
                           ,ordered = TRUE
      ),
      
      klaffetype = dplyr::recode(TypeKlaffeprotese,
                                 "CoreValve" = "CoreValve",
                                 "CoreValve Evolut R" = "CoreValve",
                                 "CoreValve Evolut PRO" = "CoreValve",
                                 "Edwards" = "Edwards",
                                 "Edwards SAPIEN 3" = "Edwards",
                                 "Edwards SAPIEN 3 Ultra" = "Edwards",
                                 "Edwards SAPIEN XT" = "Edwards",
                                 .default = "Annet"
      ),
      
      dager_mellom_prosedyre_og_utskr = as.numeric( difftime(  UtskrDato, ProsedyreDato, units = "days" ) ),
      
      NYHAKlasse = factor( NYHAKlasse,
                           levels = c(
                             "I"
                             ,"II"
                             ,"III"
                             ,"IV"
                             ,"Ukjent"
                             ,NA
                           )
                           ,exclude = NULL # inkluderer NA i levels
                           ,ordered = TRUE
      ),
      
      
      OperativTilgang = factor( OperativTilgang,
                           levels = c(
                             "A.subclavia"
                             ,"Direkte aorta"
                             ,"Transapical"
                             ,"Transfemoral"
                             ,NA
                           )
                           ,exclude = NULL # inkluderer NA i levels
                           ,ordered = TRUE
      ),
      
      
      
      # PasientKjonn = factor(PasientKjonn, 
      #                       levels = c( "Mann", "Kvinne")
      #                       ,exclude = NULL # inkluderer NA i levels
      #                       , ordered = TRUE),
      # Kommune = as.factor( Kommune ),

      PreVenstreVentrikkelFunksjon = factor( PreVenstreVentrikkelFunksjon,
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
                              ),
      
      Royker = factor(Royker, 
                       levels = c( 
                         "Aldri røykt"
                         , "Eks-røyker >1 mnd"
                         , "Røyker"
                         , "Ukjent"
                         , NA
                       )
                       ,exclude = NULL # inkluderer NA i levels
                       ,ordered = TRUE
      ),
      
      ScreeningBeslutning = factor(ScreeningBeslutning, 
                       levels = c( 
                         "TAVI"
                         , "BAV"
                         , "TAVI + PCI"
                         , NA
                       )
                       ,exclude = NULL # inkluderer NA i levels
                       ,ordered = TRUE
      ),
      
      Sykehusnavn = as.ordered( Sykehusnavn )
      
    )
  
  
  # Utledete variabler:
  AK %<>% 
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
  
  
  
  # Droppe denne kommende snutten da det kan være nyttig for NORIC om sykehus som ikke
  # utfører TAVI oppdager selv at de ved en feil har registrert noen forløp som TAVI?
  
  # # FJERNER SYKEHUS SOM IKKE UTFØRER TAVI:
  # 
  # AortaKlaffVar %<>%
  #   filter( 
  #     # Fjerner ev. rader fra Ahus, SUS og Sørlandet:
  #     Sykehusnavn %not_in% c( "Ahus", "SUS", "Sørlandet")
  #   )
  
  
  
  AK
  
}