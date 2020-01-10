#' getAKData provides local or national reg data from AortaklaffVar
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
      ,FodselsDato = ymd( FodselsDato )
      ,HovedDato = ymd( HovedDato )
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
  # (ikke fullstendig, må legge til mer etter hvert)
  AK %<>%
    mutate(
      
      Aktiviteter = addNA( Aktiviteter, ifany = TRUE )
      ,AndrePlatehemmereVedUtskrivelse = addNA( AndrePlatehemmereVedUtskrivelse , ifany = TRUE )
      ,Anestesi = addNA( Anestesi , ifany = TRUE )
      ,AngstDepresjon = addNA( AngstDepresjon , ifany = TRUE )
      ,AnnenAlvorligSykdom = addNA( AnnenAlvorligSykdom , ifany = TRUE )
      ,AntikoagulantiaVedUtskrivelse = addNA( AntikoagulantiaVedUtskrivelse , ifany = TRUE )
      
      ,Aortaforkalk = factor( Aortaforkalk,
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
      )
      
      ,ASAVedUtskrivelse = addNA( ASAVedUtskrivelse , ifany = TRUE )
      ,Atrieflimmer = addNA( Atrieflimmer , ifany = TRUE )
      ,AvdKompAnnenKomp = addNA( AvdKompAnnenKomp , ifany = TRUE )
      ,AvdKompAtrieflimmer = addNA( AvdKompAtrieflimmer , ifany = TRUE )
      ,AvdKompBlodning = addNA( AvdKompBlodning , ifany = TRUE )
      ,AvdKompBlodningGrad = addNA( AvdKompBlodningGrad , ifany = TRUE )
      ,AvdKompDialyse = addNA( AvdKompDialyse , ifany = TRUE )
      ,AvdKompDod = addNA( AvdKompDod , ifany = TRUE )
      ,AvdKompHjerneslag = addNA( AvdKompHjerneslag , ifany = TRUE )
      ,AvdKompHjerneslagGrad = addNA( AvdKompHjerneslagGrad , ifany = TRUE )
      ,AvdKompHjerteinfarkt = addNA( AvdKompHjerteinfarkt , ifany = TRUE )
      ,AvdKompInfeksjon = addNA( AvdKompInfeksjon , ifany = TRUE )
      ,AvdKomplikasjon = addNA( AvdKomplikasjon , ifany = TRUE )
      ,AvdKompPacemaker = addNA( AvdKompPacemaker , ifany = TRUE )
      ,AvdKompTamponade = addNA( AvdKompTamponade , ifany = TRUE )
      ,AvdKompTIA = addNA( AvdKompTIA , ifany = TRUE )
      ,AvdKompVaskular = addNA( AvdKompVaskular , ifany = TRUE )
      
      ,BasisRegStatus = addNA( BasisRegStatus , ifany = TRUE )
      ,BehHypertoni = addNA( BehHypertoni , ifany = TRUE )
      
      ,CanadianClass = factor( CanadianClass,
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
      )
      
      ,Diabetes = addNA( Diabetes , ifany = TRUE )
      ,DialyseFoerOp = addNA( DialyseFoerOp , ifany = TRUE )
      ,ErCTForetatt = addNA( ErCTForetatt , ifany = TRUE )
      ,ForlopsType1 = addNA( ForlopsType1 , ifany = TRUE )
      
      ,ForlopsType2 = factor( ForlopsType2,
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
      
      ,Gangtest = addNA( Gangtest , ifany = TRUE )
      ,Gripestyrke = addNA( Gripestyrke , ifany = TRUE )
      
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
      
      
      ,Hygiene = factor( Hygiene,
                         levels = c(
                           "En (1)"
                           , "To (2)"
                           , "Tre (3)"
                           ,NA
                         )
                         ,exclude = NULL # inkluderer NA i levels
                         ,ordered = TRUE )
      
      ,Indikasjon = factor( Indikasjon,
                           levels = c(
                             "Stenose"
                             ,"Stenose og insuffisiens"
                             ,"Insuffisiens"
                             ,NA
                           )
                           ,exclude = NULL # inkluderer NA i levels
                           ,ordered = TRUE
      )
      
      ,InfarktSiste90d = factor( InfarktSiste90d,
                                 levels = c(
                                   "Ja"
                                   ,"Nei"
                                   ,NA
                                 )
                                 ,exclude = NULL # inkluderer NA i levels
                                 ,ordered = TRUE
      )
      
      ,Insulin = addNA( Insulin , ifany = TRUE )
      ,Karlukning = addNA( Karlukning , ifany = TRUE )
      ,KlaffearealIkkeBeregnet = addNA( KlaffearealIkkeBeregnet , ifany = TRUE )
      ,KlaffIKlaff = addNA( KlaffIKlaff , ifany = TRUE )
      ,KOLS = addNA( KOLS , ifany = TRUE )
      ,Kommune = addNA( Kommune , ifany = TRUE )
      ,Kontrastmiddel = addNA( Kontrastmiddel , ifany = TRUE )
      ,KritiskPreopTilstand = addNA( KritiskPreopTilstand , ifany = TRUE )
      
      ,LabKompAkuttKlaff = addNA( LabKompAkuttKlaff , ifany = TRUE )
      ,LabKompAkuttVaskular = addNA( LabKompAkuttVaskular , ifany = TRUE )
      ,LabKompAnestesi = addNA( LabKompAnestesi , ifany = TRUE )
      ,LabKompAnnenKomp = addNA( LabKompAnnenKomp , ifany = TRUE )
      ,LabKompArytmi = addNA( LabKompArytmi , ifany = TRUE )
      ,LabKompBlodning = addNA( LabKompBlodning , ifany = TRUE )
      ,LabKompDod = addNA( LabKompDod , ifany = TRUE )
      ,LabKompEmboli = addNA( LabKompEmboli , ifany = TRUE )
      ,LabKompHLMaskin = addNA( LabKompHLMaskin , ifany = TRUE )
      ,LabKomplikasjon = addNA( LabKomplikasjon , ifany = TRUE )
      ,LabKompNeurologi = addNA( LabKompNeurologi , ifany = TRUE )
      ,LabKompOkklusjon = addNA( LabKompOkklusjon , ifany = TRUE )
      ,LabKompProtese = addNA( LabKompProtese , ifany = TRUE )
      ,LabKompTamponade = addNA( LabKompTamponade , ifany = TRUE )
      ,LabKompVaskular = addNA( LabKompVaskular , ifany = TRUE )
      ,Labnr = addNA( Labnr , ifany = TRUE )
      
      ,Malignitet = addNA( Malignitet , ifany = TRUE )
      ,Mitralisinsuffisiens = addNA( Mitralisinsuffisiens , ifany = TRUE )
      ,Mobilitet = addNA( Mobilitet , ifany = TRUE )
      
      ,NYHAKlasse = factor( NYHAKlasse,
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
      )
      
      ,OperativTilgang = factor( OperativTilgang,
                                 levels = c(
                                   "A.subclavia"
                                   ,"Direkte aorta"
                                   ,"Transapical"
                                   ,"Transfemoral"
                                   ,NA
                                 )
                                 ,exclude = NULL # inkluderer NA i levels
                                 ,ordered = TRUE
      )
      
      ,Pacemaker = addNA( Pacemaker , ifany = TRUE )
      ,ParavalvularLekkasje = addNA( ParavalvularLekkasje , ifany = TRUE )
      
      ,PasientKjonn = factor(PasientKjonn,
                             levels = c( "Mann", "Kvinne")
                             ,exclude = NULL # inkluderer NA i levels
                             , ordered = TRUE
      )
      
      ,PeriferKarsykdom = addNA( PeriferKarsykdom , ifany = TRUE )
      ,PerkKlaffPgaPasient = addNA( PerkKlaffPgaPasient , ifany = TRUE )
      ,PerkKlaffPgaRisiko = addNA( PerkKlaffPgaRisiko , ifany = TRUE )
      ,PerkKlaffPgaRisikoACB = addNA( PerkKlaffPgaRisikoACB , ifany = TRUE )
      ,PerkKlaffPgaRisikoAlder = addNA( PerkKlaffPgaRisikoAlder , ifany = TRUE )
      ,PerkKlaffPgaRisikoSpesiell = addNA( PerkKlaffPgaRisikoSpesiell , ifany = TRUE )
      ,PerkKlaffPgaRisikoSpiro = addNA( PerkKlaffPgaRisikoSpiro , ifany = TRUE )
      ,Porselenaorta = addNA( Porselenaorta , ifany = TRUE )
      
      ,PostAortainsuffisiens = addNA( PostAortainsuffisiens , ifany = TRUE )
      ,Postdilatasjon = addNA( Postdilatasjon , ifany = TRUE )
      ,PostMitralisinsuffisiens = addNA( PostMitralisinsuffisiens , ifany = TRUE )
      
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
      
      ,PreAortainsuffisiens = addNA( PreAortainsuffisiens , ifany = TRUE )
      ,Predilatasjon = addNA( Predilatasjon , ifany = TRUE )
      ,PreMitralisinsuffisiens = addNA( PreMitralisinsuffisiens , ifany = TRUE )
      
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
      

      ,ProtectionDevice = addNA( ProtectionDevice , ifany = TRUE )
      ,RapidPacing = addNA( RapidPacing , ifany = TRUE )
      ,RedusertMobilitet = addNA( RedusertMobilitet , ifany = TRUE )
      
      ,Royker = factor(Royker, 
                       levels = c( 
                         "Aldri røykt"
                         , "Eks-røyker >1 mnd"
                         , "Røyker"
                         , "Ukjent"
                         , NA
                       )
                       ,exclude = NULL # inkluderer NA i levels
                       ,ordered = TRUE
      )
      
      ,ScreeningBeslutning = factor(ScreeningBeslutning, 
                                    levels = c( 
                                      "TAVI"
                                      , "BAV"
                                      , "TAVI + PCI"
                                      , NA
                                    )
                                    ,exclude = NULL # inkluderer NA i levels
                                    ,ordered = TRUE
      )
      
      ,SkjemaStatus = addNA( SkjemaStatus , ifany = TRUE )
      ,SmerterUbehag = addNA( SmerterUbehag , ifany = TRUE )
      ,Steroidbehandling = addNA( Steroidbehandling , ifany = TRUE )
      ,Stralebehandling = addNA( Stralebehandling , ifany = TRUE )
      ,Sykehusnavn = as.ordered( Sykehusnavn )
      
      ,Thoraxdeformitet = addNA( Thoraxdeformitet , ifany = TRUE )
      ,TidlACB = addNA( TidlACB , ifany = TRUE )
      ,TidlAnnet = addNA( TidlAnnet , ifany = TRUE )
      ,TidlAVR = addNA( TidlAVR , ifany = TRUE )
      ,TidlHjerneslag = addNA( TidlHjerneslag , ifany = TRUE )
      ,TidlHjerteoperasjon = addNA( TidlHjerteoperasjon , ifany = TRUE )
      ,TidlMitralplastikk = addNA( TidlMitralplastikk , ifany = TRUE )
      ,TidlMVR = addNA( TidlMVR , ifany = TRUE )
      ,TidlPCI = addNA( TidlPCI , ifany = TRUE )
      ,UgunstigAnatomi = addNA( UgunstigAnatomi , ifany = TRUE )
      
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
      
      ,VellykketProsedyre = addNA( VellykketProsedyre , ifany = TRUE )
      
    )
  
  
  
  # Utledete variabler:
  AK %<>% 
    mutate( 
      
      dager_mellom_prosedyre_og_utskr = as.numeric( difftime(  UtskrDato, ProsedyreDato, units = "days" ) )
      
      ,klaffetype = dplyr::recode(TypeKlaffeprotese,
                                  "CoreValve" = "CoreValve",
                                  "CoreValve Evolut R" = "CoreValve",
                                  "CoreValve Evolut PRO" = "CoreValve",
                                  "Edwards" = "Edwards",
                                  "Edwards SAPIEN 3" = "Edwards",
                                  "Edwards SAPIEN 3 Ultra" = "Edwards",
                                  "Edwards SAPIEN XT" = "Edwards",
                                  .default = "Annet"
      )
      
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
      # kvartal = as.factor( gsub( "\\.", "-", kvartal) )
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